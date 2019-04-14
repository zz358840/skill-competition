//104年電腦修復範例


/*
This example should run on most AVRs with only little changes. No special
hardware resources except INT0 are used. You may have to change usbconfig.h for
different I/O pins for USB. Please note that USB D+ must be the INT0 pin, or
at least be connected to INT0 as well.
*/
#include <avr/io.h>
#include <avr/wdt.h>
#include <avr/interrupt.h>  /* for sei() */
#include <util/delay.h>     /* for _delay_ms() */
#include <avr/eeprom.h>

#include <avr/pgmspace.h>   /* required by usbdrv.h */
#include "usbdrv.h"
#include "oddebug.h"        /* This is also an example for using debug macros */

#define LED_NUM  8 

/* 全域變數 */
uchar page, mode, var1, renew ,temp_r,temp_g,temp_b, in_db;
unsigned int timer[2];
uchar pixels[48];        // Holds LED color values (3 bytes each)
uchar pixels_usb[24];

void process_show(void);
void setPixelColor( uchar number, uchar color_r, uchar color_g, uchar color_b);
void colorWipe(uchar r, uchar g, uchar b, uchar wait);
void rainbow(uchar wait); 
void Wheel(uchar WheelPos);

/* ----------------------------- USB interface ----------------------------- */
PROGMEM const char usbHidReportDescriptor[22] = {    /* USB report descriptor */
    0x06, 0x00, 0xff,              // USAGE_PAGE (Generic Desktop)
    0x09, 0x01,                    // USAGE (Vendor Usage 1)
    0xa1, 0x01,                    // COLLECTION (Application)
    0x15, 0x00,                    //   LOGICAL_MINIMUM (0)
    0x26, 0xff, 0x00,              //   LOGICAL_MAXIMUM (255)
    0x75, 0x08,                    //   REPORT_SIZE (8)
    0x95, 0x80,                    //   REPORT_COUNT (128)
    0x09, 0x00,                    //   USAGE (Undefined)
    0xb2, 0x02, 0x01,              //   FEATURE (Data,Var,Abs,Buf)
    0xc0                           // END_COLLECTION
};
/* Since we define only one feature report, we don't use report-IDs (which
 * would be the first byte of the report). The entire report consists of 128
 * opaque data bytes.
 */

/* The following variables store the status of the current data transfer */
static uchar    currentAddress;
static uchar    bytesRemaining;

/* usbFunctionRead() is called when the host requests a chunk of data from
 * the device. For more information see the documentation in usbdrv/usbdrv.h.
 */
uchar   usbFunctionRead(uchar *data, uchar len)
{
   if(len > bytesRemaining)
      len = bytesRemaining;
 	 if(currentAddress == 0)
 	 {
 
    *(data)   = 0;  	// Key value                         
 		*(data+1) = 0;		// Reserve (zero)
 		*(data+2) = 0;		// Reserve (zero)
 		*(data+3) = 0;		// Reserve (zero)
 		*(data+4) = 0;		// Reserve (zero)
 		*(data+5) = 0;		// Reserve (zero)
 		*(data+6) = 0;		// Reserve (zero)
 		*(data+7) = 0;		// Reserve (zero)
   }
   currentAddress += len;
   bytesRemaining -= len;
   
   return len;
}

/* usbFunctionWrite() is called when the host sends a chunk of data to the
 * device. For more information see the documentation in usbdrv/usbdrv.h.
 */
uchar   usbFunctionWrite(uchar *data, uchar len)
{
	 
   if(bytesRemaining == 0)
      return 1;               /* end of transfer */
   if(len > bytesRemaining)
      len = bytesRemaining;
      
   if(currentAddress == 0)
   {         
      if(*(data+1) < LED_NUM)                       //*(data+1) led編號 
      {  
         pixels_usb[*(data+1) * 3]     = *(data+2); //R   	     	
         pixels_usb[*(data+1) * 3 + 1] = *(data+3); //G   	     	
         pixels_usb[*(data+1) * 3 + 2] = *(data+4); //B   	     	                  	
      }
      
      if(*(data+1) ==  7)
      {
         mode  = 2;
         renew = 1;
      }   
        
 	 }
   currentAddress += len;
   bytesRemaining -= len;
   return bytesRemaining == 0; /* return 1 if this was the last chunk */
}

usbMsgLen_t usbFunctionSetup(uchar data[8])
{
	usbRequest_t    *rq = (void *)data;
    if((rq->bmRequestType & USBRQ_TYPE_MASK) == USBRQ_TYPE_CLASS){    /* HID class request */
        if(rq->bRequest == USBRQ_HID_GET_REPORT){  /* wValue: ReportType (highbyte), ReportID (lowbyte) */
            /* since we have only one report type, we can ignore the report-ID */
            bytesRemaining = 8;
            currentAddress = 0;
            return USB_NO_MSG;  /* use usbFunctionRead() to obtain data */
        }else if(rq->bRequest == USBRQ_HID_SET_REPORT){
            /* since we have only one report type, we can ignore the report-ID */
            bytesRemaining = 8;
            currentAddress = 0;
            return USB_NO_MSG;  /* use usbFunctionWrite() to receive data from host */
        }
    }else{
        /* ignore vendor type requests, we don't use any */
    }
    return 0;
}
/* ------------------------------------------------------------------------- */
ISR (TIMER0_OVF_vect)                       // timer0 overflow interrupt
{
   uchar   i;
   // event to be exicuted every 5ms here
   TCNT0 = 22;  
      
   for(i=0;i<2;i++)
   {
      if(timer[i]>0)
         timer[i]--;           
   }    
}

int main(void)
{	
	 uchar   i;	
   wdt_enable(WDTO_1S);
   /* Even if you don't use the watchdog, turn it off here. On newer devices,
     * the status of the watchdog (on/off, period) is PRESERVED OVER RESET!
   */
        
   {                  /* IO SETUP */  
     	                //   DDB7   DDB6   DDB5   DDB4   DDB3   DDB2   DDB1   DDB0
     	DDRB = 0x3f;    //    0      0      1      1      1      1      1      1
     	                //    x      x     out    out    out    out    out    out
                      //  PORTB7 PORTB6 PORTB5 PORTB4 PORTB3 PORTB2 PORTB1 PORTB0
     	PORTB = 0x00;   //    0      0      0      0      0      0      0      0
       
     				          //    -     DDC6   DDC5   DDC4   DDC3   DDC2   DDC1   DDC0
     	DDRC = 0x78;    //    0      1      1      1      1      0      0      0  
     					        //  (Read)  out    out    out    out    in      in    in
                      //    -    PORTC6 PORTC5 PORTC4 PORTC3 PORTC2 PORTC1 PORTC0							 
      PORTC = 0x07;   //    0      0      0      0      0      0      0      0
  
     	                //   DDD7   DDD6   DDD5   DDD4   DDD3   DDD2   DDD1   DDD0
     	DDRD = 0xea;    //    1      1      1      0      1      0      1      0 
     					        //   out    out    out    D-     out     D+    out    in
     	                //  PORTD7 PORTD6 PORTD5 PORTD4 PORTD3 PORTD2 PORTD1 PORTD0
     	PORTD = 0x01;   //    0      0      0      0      0      0      0      1  
   }
	
   usbInit();
   usbDeviceDisconnect();  /* enforce re-enumeration, do this while interrupts are disabled! */
	 i = 0;
   while(--i)              /* fake USB disconnect for > 250 ms */
   {             
      wdt_reset();
      _delay_ms(1);
   }
   usbDeviceConnect();
   sei();
   
   TIMSK |= (1 << TOIE0);
   TCCR0 |= (1 << CS02);             // set prescaler to 256 and start the timer 
   TCNT0 = 128;                   

   /* 變數初始設定 */ 
 	 mode   = 0;    
   page   = 0;	                               
   var1   = 0;
   renew  = 0;
   in_db  = 0;
   for(i=0;i<4;i++)
      timer[i] = 0;  
   
   for(;;)          /* main event loop */
   {                
      wdt_reset();
      usbPoll();		
      
      if(mode == 0)
      {
   		  if(timer[0] ==0)                          //5ms x 200 = 1s
   		  {                                        
     		   timer[0] = 200;
              
            if(page == 0)
            {        
               setPixelColor(0,255, 0, 0);                 
               setPixelColor(1,0, 255, 0);   
               setPixelColor(2,0, 0, 255);
               setPixelColor(3,255, 0, 255);            
               setPixelColor(4,255, 255, 0);
               setPixelColor(5,0, 255, 255);                                                            
               setPixelColor(6,255, 255, 255);
               setPixelColor(7,100, 100, 100);                                                                                                                                                            
               process_show();                           
            } 
            else if(page == 1)
               colorWipe(255, 0, 0, 0);    
            else if(page == 2)
               colorWipe(0, 255, 0, 0);                                 
            else if(page == 3)
               colorWipe(0, 0, 255, 0);                                      
               
            page++;
            page = page % 4;                                       
   		  }        
      }  
      else if(mode == 1)
      {
   		  if(timer[0] ==0)                          //5ms x 1 = 5ms
   		  {                                        
     		   timer[0] = 4;    
                                    
            rainbow(20);                 
   		  }		          
      }  
      else if((mode == 2)&&(renew))        //USB控制顏色
      {
         renew = 0; 
         for(i=0; i<LED_NUM; i++)
            setPixelColor(i,pixels_usb[i*3], pixels_usb[i*3+1], pixels_usb[i*3+2]);                                                                                                                                                                            
         process_show();        
      }   		
      
      if(timer[1] == 0)                    //10ms 檢查一次按鍵
      {
         timer[1] = 2;  
         
         if((PIND & 0x01)==0)              //按鍵按下
         {
            if(in_db < 100)                //避免溢位
               in_db++; 
            if(in_db == 3)                 //按鍵按下30ms才算
            {
               mode++;
               mode = mode % 2;  
            }
         }
         else
            in_db = 0;           

      }                  
         			                                       
   }
   return 0;
}


/* ------------------------------------------------------------------------- */
void process_show(void)
{
   volatile uint16_t
   loop_cnt = LED_NUM * 3; // Loop counter = 8(led number) * 3
   volatile uint8_t
   *ptr = pixels,   // Pointer to next byte
   b_data   = *ptr++,    // Current byte value
   hi,              // PORT w/output bit set high
   lo;              // PORT w/output bit set low
   volatile uint8_t next;

   cli();
   hi   = PORTB |  0x01;
   lo   = PORTB & ~0x01;
   next = lo;
   if(b_data & 0x80) next = hi;
                        
   asm volatile               //1T ~= 83.3ns 
   (                          // 21 instruction clocks per bit: HHHHxxxxxxxxxxxxLLLLL
    "headD:"                   "\n\t" //        (T =  0)
     "out   %[port], %[hi]"    "\n\t" //        (T =  1)
     "rcall bitTimeD"          "\n\t" // 3      Bit 7  (T = 15)
     "out   %[port], %[hi]"    "\n\t"
     "rcall bitTimeD"          "\n\t" // Bit 6
     "out   %[port], %[hi]"    "\n\t"
     "rcall bitTimeD"          "\n\t" // Bit 5
     "out   %[port], %[hi]"    "\n\t"
     "rcall bitTimeD"          "\n\t" // Bit 4
     "out   %[port], %[hi]"    "\n\t"
     "rcall bitTimeD"          "\n\t" // Bit 3
     "out   %[port], %[hi]"    "\n\t"
     "rcall bitTimeD"          "\n\t" // Bit 2
     "out   %[port], %[hi]"    "\n\t"
     "rcall bitTimeD"          "\n\t" // Bit 1
     // Bit 0:
     "out  %[port] , %[hi]"    "\n\t" // 1    PORT = hi    (T =  1)
     "rjmp .+0"                "\n\t" // 2    nop nop      (T =  3)
     "ld   %[byte] , %a[ptr]+" "\n\t" // 2    b = *ptr++   (T =  5)
     "out  %[port] , %[next]"  "\n\t" // 1    PORT = next  (T =  6)
     
     "nop"                     "\n\t" // 1                 (T =  7)  +   
     "nop"                     "\n\t" // 1                 (T =  8)  +   
     "nop"                     "\n\t" // 1                 (T =  9)  +         
     "nop"                     "\n\t" // 1                 (T = 10)  +               
     "nop"                     "\n\t" // 1                 (T = 11)  +                     
     "nop"                     "\n\t" // 1                 (T = 12)  +     
     
     "mov  %[next] , %[lo]"    "\n\t" // 1    next = lo    (T = 13)
     "sbrc %[byte] , 7"        "\n\t" // 1-2  if(b & 0x80) (T = 14)
      "mov %[next] , %[hi]"    "\n\t" // 0-1    next = hi  (T = 15)
     "nop"                     "\n\t" // 1                 (T = 16)
     "out  %[port] , %[lo]"    "\n\t" // 1    PORT = lo    (T = 17)
     "sbiw %[count], 1"        "\n\t" // 2    i--          (T = 19)
     "brne headD"              "\n\t" // 2    if(i != 0) -> (next byte)(T = 21)
      "rjmp doneD"             "\n\t"
     "bitTimeD:"               "\n\t" //                      (T =  4)
      "out  %[port], %[next]"  "\n\t" // 1    PORT = next     (T =  5)    
      "nop"                    "\n\t" // 1                    (T =  6)    
      "nop"                    "\n\t" // 1                    (T =  7)     
      "nop"                    "\n\t" // 1                    (T =  8)           
      "nop"                    "\n\t" // 1                    (T =  9)                 
      "nop"                    "\n\t" // 1                    (T = 10)                       
      "nop"                    "\n\t" // 1                    (T = 11)                 
      "mov  %[next], %[lo]"    "\n\t" // 1    next = lo       (T = 12)    
      "rol  %[byte]"           "\n\t" // 1    b <<= 1         (T = 13)    
      "sbrc %[byte], 7"        "\n\t" // 1-2  if(b & 0x80)    (T = 14)    
      "mov %[next], %[hi]"     "\n\t" // 0-1   next = hi      (T = 15)
      "nop"                    "\n\t" // 1                    (T = 16)
      "out  %[port], %[lo]"    "\n\t" // 1    PORT = lo       (T = 17)
      "ret"                    "\n\t" // 4    nop nop nop nop (T = 18-21)
      "doneD:"                 "\n"
     : [byte]  "+r" (b_data),
       [next]  "+r" (next),
       [count] "+w" (loop_cnt)
     : [port]   "I" (_SFR_IO_ADDR(PORTB)),
       [ptr]    "e" (ptr),
       [hi]     "r" (hi),
       [lo]     "r" (lo)
    );
    sei();    
}

void setPixelColor( uchar number, uchar color_r, uchar color_g, uchar color_b)
{
   uchar *p;  
   if(number < LED_NUM) 
   {
      p = &pixels[number * 3];
      p[0] = color_r;
      p[1] = color_g;
      p[2] = color_b;
   }
}

void colorWipe(uchar r, uchar g, uchar b, uchar wait)        //設定全部的LED顏色
{
   uchar i;
   for(i=0; i<LED_NUM; i++)
   {
      setPixelColor(i,r, g, b);
   }    
   process_show();

//   delay(wait);
   
}

void rainbow(uchar wait) 
{
   uchar i;
 
   for(i=0; i<LED_NUM; i++) 
   {      
      Wheel((i+var1) & 255);
      setPixelColor(i,temp_r,temp_g,temp_b);
   }
   process_show();
//   delay(wait);
   var1++;
}

void Wheel(uchar WheelPos) 
{
   WheelPos = 255 - WheelPos;
   if(WheelPos < 85) 
   {  
      temp_r = 255 - WheelPos * 3;
      temp_g = 0;
      temp_b = WheelPos * 3;      
   } 
   else if(WheelPos < 170) 
   {
      WheelPos -= 85;
      temp_r = 0;
      temp_g = WheelPos * 3;
      temp_b = 255 - WheelPos * 3;      
   } 
   else
   {
      WheelPos -= 170;
      
      temp_r = WheelPos * 3;
      temp_g = 255 - WheelPos * 3;
      temp_b = 0;             
   }
}
