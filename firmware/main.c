//104年電腦修復
//接腳說明
//PB0=S1,PB1=S2,,PB2=S3,PB3=S4,PB4=S5,PB5=S6
//PD6=S7,PD7=S8
//PD2=D+
//PD4=D-
//PC0=led8的Din
//PC5=可變電組
//更新日期:10/26
//更新日誌:最後格式化韌體程式碼及VB程式碼
//功能說明:
//三種模式：開機模式 按鈕模式 USB模式

/*通電 VB不執行*/
//開機模式：紅綠藍黑 四個顏色持續閃爍 中間延遲1秒
//按鈕模式：分為單一顏色與跑馬燈功能
//單一顏色：每一個按鈕都能設定對應的led的顏色
//跑馬燈：一個按鈕就一個跑馬燈功能

/*通電 VB執行*/
//會一直同步ADC/電壓/key
//VB關閉後 板子會跳回開機模式
//開機模式：VB會完全同步板子的顏色 key=空白
//按鈕模式：VB會完全同步板子的顏色
//單一顏色：VB會完全同步板子的顏色
//跑馬燈：VB會完全同步板子的顏色
//USB模式：用VB直接控制板子 也可分為單一顏色與跑馬燈 key=空白

/*
This example should run on most AVRs with only little changes. No special
hardware resources except INT0 are used. You may have to change usbconfig.h for
different I/O pins for USB. Please note that USB D+ must be the INT0 pin, or
at least be connected to INT0 as well.
*/
#include <avr/io.h>
#include <avr/wdt.h>
#include <avr/interrupt.h>  /* sei():致能中斷 */
#include <util/delay.h>     /*  _delay_ms() */
#include <avr/eeprom.h>
#include <avr/pgmspace.h>   /* usbdrv.h所需的參數定義 */
#include "usbdrv.h"
#include "oddebug.h"        /* 使用除錯巨集的範例 */
#include <math.h> 			/* 數學函數 fabs():絕對值 */

#define LED_NUM  8

/* 全域變數 */
//page:開機模式的狀態選擇
//mode:模式選擇
//var1:給rainbow使用的變數
//renew:配合mode進入USB模式
//temp_r, temp_g, temp_b:彩虹漸層的rgb
//in_db:判斷按鈕及避免溢位的變數
//key:按鈕變數
uchar page, mode, var1, renew , temp_r, temp_g, temp_b, in_db, key;

int data_state;  //data_state:目前是否有資料(0沒有 1有)

/* 單一顏色 變數區 */
//led:setPixelcolor的led(7=led1 6=led2 ... 0=led8)
//color:單一顏色設定的變數
//ledcolor[8]:將目前選擇的顏色丟給color //led[0]:S1 led[1]:S2 ... led[7]:S8
int led, color, ledcolor[8];

/* 跑馬燈 變數區 */
//runled:跑馬燈的led變數
//runr,rung,runb:跑馬燈的顏色
//usbreadnum:在runled往下減之前將現有值傳給usbreadnum
int runled, runr, rung, runb, usbreadnum;

//usbwritenum:usb寫入led編號並x4倍(配合pixels_usb[32])
//pixels_usb[usbwritenum]:led
//pixels_usb[usbwritenum + 1]:r
//pixels_usb[usbwritenum + 2]:g
//pixels_usb[usbwritenum + 3]:b
int usbwritenum;

uchar *p;  //*p:給setPixelColor函式用的指標變數

unsigned int timer[3];  //幾秒偵測時間(5ms*X)
uchar pixels[48];  // LED用的陣列(每顆LED 3byte)
uchar pixels_usb[32];  //USB用的陣列
static uchar AdcData[2] = {0,0};  //ADC值 L:AdcData[0] H:AdcData[1]
int adc_to_color=0; //將ADC值轉回設定RGB的值(0~255)

void process_show(void);
void setPixelColor(uchar number, uchar color_r, uchar color_g, uchar color_b);
void colorWipe(uchar r, uchar g, uchar b, uchar wait);
void rainbow(uchar wait);
void Wheel(uchar WheelPos);
void button_click(void);
void setcolor(void);
void cleandata(void);
void color_select(void);
void getledset(void);
void button_runhorse_function(void);
void setrunhorse(void);
void button_color_set_function(void);
void runhorse_old_led(void);

void setAdc(void)  //設定ADC
{
									//通道選擇 MUX3..0=0000-->ADC0,0001-->ADC1,0010-->ADC2
	ADMUX |= (0<<MUX3) | (1<<MUX2) | (0<<MUX1) | (1<<MUX0);	//0011-->ADC3,0100-->ADC4,0101-->ADC5,0110-->ADC6
									//0111-->ADC7,XX,1110-->1.23V,1111-->0V
	ADMUX |= (1<< REFS0) | (0<< REFS1); // 設定ADC參考電壓 REFS1 REFS0=00-->AREF,01-->AVCC,10-->X,11-->2.56V	
//	ADMUX |= (1 << ADLAR); // 設定輸出對齊方式 ADLAR=0-->靠右,1-->靠左(8bit時由ADCH取值)
															// 設定 ADC 預分頻
	ADCSRA |= (1<< ADPS2) | (1<< ADPS1) | (1<< ADPS0); 	//ADPS2..0 =000、001-->2,010-->4,011-->8
															//100-->16,101-->32,110-->64,111-->128
		
//	ADCSRA |= (1 << ADFR); // 設定 ADC 轉換模式 ADFR=0-->單次,1-->連續
	ADCSRA |= (1 << ADEN); // ADC 致能 ADEN=0-->OFF,1--ON
}

void getAdc(void)  //取ADC值
{
	ADCSRA|=(1<<ADSC);					//Start ADC conversion ADSC=0-->關閉,1-->啟動
//	while ((ADCSRA&_BV(ADIF))==0x00){} 	//Wait to finish conversion
	while (ADCSRA&_BV(ADSC)); 	//Wait to finish conversion
	AdcData[0]=ADCL;					//讀取 ADC 的值
	AdcData[1]=ADCH;
//	ADCSRA |= (1 << ADIF);				//清除 ADIF
	adc_to_color= (256 * AdcData[1] + AdcData[0]) / 4.01; //取出來的AD為0~1023 變數為整數 除以4為0~256 因此除以大於4即可為0~255
}

/* ------------------------------ USB 介面 --------------------------------- */
PROGMEM const char usbHidReportDescriptor[22] = {   /* USB 報告描述元 */
  0x06, 0x00, 0xff,              // USAGE_PAGE (販售商自定)
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

/* 以下的變數是用來暫存目前資料傳輸的狀態 */
static uchar    currentAddress;
static uchar    bytesRemaining;

/* usbFunctionRead():當主機要從USB裝置要求大批資料時就會呼叫usbFunctionRead() */
/* 相關訊息可以可以參考usbdrv/usbdrv.h文件 */
uchar   usbFunctionRead(uchar *data, uchar len)//USB讀取的函數
{
  if (len > bytesRemaining)
    len = bytesRemaining;
  if (currentAddress == 0)
  {
    switch (mode)
    {
      //資料格式
      //*(data)   = led;  //單一顏色:led 跑馬燈:usbreadnum
      //*(data + 1) = p[0];  //單一顏色:p[0] 跑馬燈:runr
      //*(data + 2) = p[1];  //單一顏色:p[1] 跑馬燈:rung
      //*(data + 3) = p[2];  //單一顏色:p[2] 跑馬燈:runb
      //*(data + 4) = mode;	  //模式
      //*(data + 5) = key;  //按鈕
      //*(data + 6) = AdcData[0];  //ADC低電位
      //*(data + 7) = AdcData[1];  //ADC高電位
	  
    case 0:  //開機模式
      *(data)   = 0;
      *(data + 1) = p[0];
      *(data + 2) = p[1];
      *(data + 3) = p[2];
      *(data + 4) = mode;	
      *(data + 5) = key;
      *(data + 6) = AdcData[0];
      *(data + 7) = AdcData[1];
      break;
    case 1:  //按鈕模式 單一顏色
      *(data)   = led;
      *(data + 1) = p[0];
      *(data + 2) = p[1];
      *(data + 3) = p[2];
      *(data + 4) = mode;
      *(data + 5) = key;
      *(data + 6) = AdcData[0];
      *(data + 7) = AdcData[1];
      break;
    case 2:  //按鈕模式 跑馬燈
      *(data)   = usbreadnum;
      *(data + 1) = runr;
      *(data + 2) = rung;
      *(data + 3) = runb;	
      *(data + 4) = mode;
      *(data + 5) = key;
      *(data + 6) = AdcData[0];
      *(data + 7) = AdcData[1];
      break;
    case 3:  //USB模式
      //*(data)   = usbwritelednum;
      //*(data + 1) = p[0];
      //*(data + 2) = p[1];
      //*(data + 3) = p[2];
      *(data + 4) = mode;
      *(data + 5) = key;
      *(data + 6) = AdcData[0];
      *(data + 7) = AdcData[1];
      break;
    }
  }
  currentAddress += len;
  bytesRemaining -= len;
  return len;
}

/* usbFunctionWrite():當主機要送出大批資料給USB裝置時就會呼叫usbFunctionWrite()*/
/* 相關訊息可以可以參考usbdrv/usbdrv.h文件*/
uchar   usbFunctionWrite(uchar *data, uchar len)//USB寫入的函數
{
  if (bytesRemaining == 0)
    return 1;               /* end of transfer */
  if (len > bytesRemaining)
    len = bytesRemaining;
  if (currentAddress == 0)
  {
    //此處開始接收資料
    //OutDataEightByte 0, 7, 255, 255, 255, 0, 0, 0
    //VB寫入與板子讀取對應關係
    //*(data + 0):空白(0)
    //*(data + 1):led編號
    //*(data + 2):R
    //*(data + 3):G
    //*(data + 4):B
    //*(data + 5):空白(0)
    //*(data + 6):空白(0)
    //*(data + 7):usb寫入模式給板子 板子讀取後切換(暫無設定)

    if (*(data + 1) < LED_NUM)                    //*(data+1) led編號
    {
      usbwritenum = *(data + 1) * 4;
      pixels_usb[*(data + 1) * 4]     = *(data + 1); //led編號
      pixels_usb[*(data + 1) * 4 + 1] = *(data + 2); //R
      pixels_usb[*(data + 1) * 4 + 2] = *(data + 3); //G
      pixels_usb[*(data + 1) * 4 + 3] = *(data + 4); //B
      mode  = 3;
      renew = 1;
      if (*(data + 7) == 1)
      {
        mode  = 0;
        renew = 0;
      }
    }
  }
  currentAddress += len;
  bytesRemaining -= len;
  return bytesRemaining == 0; /* 如果這是最後一批資料，就回傳1 */
}

/* USB HID群組要求: GET_REPORT與SET_REPORT */
usbMsgLen_t usbFunctionSetup(uchar data[8])//USB初始設定
{
  usbRequest_t    *rq = (void *)data;
  if ((rq->bmRequestType & USBRQ_TYPE_MASK) == USBRQ_TYPE_CLASS) {  /* HID class request */
    if (rq->bRequest == USBRQ_HID_GET_REPORT) { /* wValue: ReportType (highbyte), ReportID (lowbyte) */
      /* 由於僅有一個報告類型，因此忽略報告ID */
      bytesRemaining = 8;
      currentAddress = 0;
      return USB_NO_MSG;  /* 使用usbFunctionRead()來傳送資料到PC主機*/
    } 
    else if (rq->bRequest == USBRQ_HID_SET_REPORT) {
      /* 由於僅有一個報告類型，因此忽略報告ID */
      bytesRemaining = 8;
      currentAddress = 0;
      return USB_NO_MSG;  /* 使用 usbFunctionWrite()從PC主機接收資料*/
    }
  } 
  else {
    /* 在此沒有使用販售商要求，因此忽略 */
  }
  return 0;
}
/* ------------------------------------------------------------------------- */
ISR (TIMER0_OVF_vect)                       // timer0 overflow interrupt //中斷函數
{
  uchar   i;
  // event to be exicuted every 5ms here
  TCNT0 = 20;//20
  for (i = 0; i < 3; i++)
  {
    if (timer[i] > 0)
      timer[i]--;
  }
}

int main(void)
{
  uchar   i;
  wdt_enable(WDTO_1S);
  //  wdt_disable();
  /* Even if you don't use the watchdog, turn it off here. On newer devices,
   * the status of the watchdog (on/off, period) is PRESERVED OVER RESET!
   */

   { 				// IO SETUP
					//   DDB7   DDB6   DDB5   DDB4   DDB3   DDB2   DDB1   DDB0
    DDRB = 0x00;    //    0      0      0      0      0      0      0      0
					//    x      x      in     in     in     in     in     in
					//  PORTB7 PORTB6 PORTB5 PORTB4 PORTB3 PORTB2 PORTB1 PORTB0
    PORTB = 0x3f;   //    0      0      1      1      1      1      1      1

					//    -     DDC6   DDC5   DDC4   DDC3   DDC2   DDC1   DDC0
    DDRC = 0x01;    //    0      0      0      0      0      0      0      1
					//  (Read)   x      in     x      x      x      x     out
					//    -    PORTC6 PORTC5 PORTC4 PORTC3 PORTC2 PORTC1 PORTC0
    PORTC = 0x00;   //    0      0      0      0      0      0      0      0

					//   DDD7   DDD6   DDD5   DDD4   DDD3   DDD2   DDD1   DDD0
    DDRD = 0x00;    //    0      0      0      0      0      0      0      0
					//    in     in     x      D-     x      D+     x      x
					//  PORTD7 PORTD6 PORTD5 PORTD4 PORTD3 PORTD2 PORTD1 PORTD0
    PORTD = 0xc0;   //    1      1      0      0      0      0      0      0
  }

  usbInit();
  usbDeviceDisconnect();  /* 強制重新裝置列舉，當執行此副程式時，中斷是被除能的 */
  i = 0;
  while (--i)             /* 模擬USB插頭拔出PC主機，假的脫離動作， 時間 > 250 ms */
  {
    wdt_reset(); 		   /* 重置看門狗計時器 */
    _delay_ms(1);
  }
  usbDeviceConnect();
  sei(); 		/* 致能整體中斷 */

  TIMSK |= (1 << TOIE0);            //timer 設定
  TCCR0 |= (1 << CS02);             // set prescaler to 256 and start the timer
  TCNT0 = 128;

  setAdc();

  /* 變數初始設定 */
  mode   = 0;
  page  = 0;
  var1   = 0;
  renew  = 0;
  in_db  = 0;
  key = 0;
  for (i = 0; i < 3; i++)
    timer[i] = 0;
  data_state = 0;
  led = 0;
  color = 0;
  for (i = 0; i < 8; i++)
    ledcolor[i] = 0;
  runled = 0;
  runr = 0;
  rung = 0;
  runb = 0;
  usbreadnum = 0;
  usbwritenum = 0;

  for (;;)         /* main event loop */
  {
    wdt_reset(); 		/* 重置看門狗計時器 */
    usbPoll(); 			/* 輪詢USB控制傳輸 */

    if (mode == 0) //開機模式
    {
      if (timer[0] == 0)                        //5ms x 200 = 1s
      {
        timer[0] = 199;
        key = 0;
        data_state = 1;
        if (page == 0)
        {
          colorWipe(adc_to_color, 0, 0, 0);
        }
        else if (page == 1)
          colorWipe(0, adc_to_color, 0, 0);
        else if (page == 2)
          colorWipe(0, 0, adc_to_color, 0);
        else if (page == 3)
          colorWipe(0, 0, 0, 0);
        page++;
        page = page % 4;
      }
    }

    else if (mode == 1) //按鈕模式 單一顏色
    {
      if (timer[0] == 0)                        //5ms x 2 = 10ms
      {
        timer[0] = 2;//4
        button_color_set_function();
      }
    }

    else if (mode == 2) //按鈕模式 跑馬燈
    {
      if (timer[0] == 0)                        //5ms x 199 = 995ms
      {
        timer[0] = 199;
        button_runhorse_function();
      }
    }

    else if ((mode == 3) && (renew)) //USB模式
    {
      renew = 0;
      key = 0;
      setPixelColor(pixels_usb[usbwritenum], pixels_usb[usbwritenum + 1], pixels_usb[usbwritenum + 2], pixels_usb[usbwritenum + 3]);
      process_show();
    }

    if (timer[1] == 0) //10ms 檢查一次按鍵
    {
      timer[1] = 2;
      button_click();
    }

    if (timer[2] == 0) //25ms 讀取ADC
    {
      timer[2] = 5;
      getAdc();
    }

  }
  return 0;
}


/* ------------------------------------------------------------------------- */
void process_show(void)//setPixecolor設定後要用此副程式來顯示
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
  hi   = PORTC |  0x01;
  lo   = PORTC & ~0x01;
  next = lo;
  if (b_data & 0x80) next = hi;

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
: 
    [byte]  "+r" (b_data),
  [next]  "+r" (next),
  [count] "+w" (loop_cnt)
: 
    [port]   "I" (_SFR_IO_ADDR(PORTC)),
  [ptr]    "e" (ptr),
  [hi]     "r" (hi),
  [lo]     "r" (lo)
    );
  sei();
}

void setPixelColor(uchar number, uchar color_r, uchar color_g, uchar color_b) //設定單一LED顏色
{
  if (number < LED_NUM)
  {
    p = &pixels[number * 3];
    p[0] = color_r;
    p[1] = color_g;
    p[2] = color_b;
  }
}

void colorWipe(uchar r, uchar g, uchar b, uchar wait) //設定全部的LED顏色
{
  uchar i;
  for (i = 0; i < LED_NUM; i++)
  {
    setPixelColor(i, r, g, b);
  }
  process_show();
  //   delay(wait);
}

void rainbow(uchar wait) //彩虹漸層
{
  uchar i;
  for (i = 0; i < LED_NUM; i++)
  {
    Wheel((i + var1) & 255);
    setPixelColor(i, temp_r, temp_g, temp_b);
  }
  process_show();
  //   delay(wait);
  var1++;
}

void Wheel(uchar WheelPos) //給rainbow用的顏色暫存設定
{
  WheelPos = 255 - WheelPos;
  if (WheelPos < 85)
  {
    temp_r = 255 - WheelPos * 3;
    temp_g = 0;
    temp_b = WheelPos * 3;
  }
  else if (WheelPos < 170)
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

void button_click(void) //判斷按鈕
{
  if (((PINB & 0x01) == 0) && ((PINB & 0x02) == 0))           //S1和S2按鈕都按下
  {
    if (in_db < 100)               	  //避免溢位
      in_db++;
    if (in_db == 1)                	  //按鍵按下10ms才算
    {
      mode = 0;
      cleandata();
    }
  }

  else
  {
    if ((PINB & 0x01) == 0)          //S1按鈕按下
    {
      if (in_db < 100)               //避免溢位
        in_db++;
      if (in_db == 1)                //按下10ms才算
      {
        key = 1;
        /*--單一按鈕顏色設定--*/
        led = 7;
        ledcolor[0]++;
        color = ledcolor[0];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---單一按鈕跑馬燈---*/
        /*
        runled = 7;
        runr = 255;
        rung = 0;
        runb = 0;
        mode = 2;
        data_state = 1;*/
        /*--------------------*/
      }
    }

    if ((PINB & 0x02) == 0)          //S2按鈕按下
    {
      if (in_db < 100)               //避免溢位
        in_db++;
      if (in_db == 1)                //按下10ms才算
      {
        key = 2;
        /*--單一按鈕顏色設定--*/
        led = 6;
        ledcolor[1]++;
        color = ledcolor[1];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---單一按鈕跑馬燈---*/
        /*
        runled = 7;
        runr = 0;
        rung = 255;
        runb = 0;
        mode = 2;
        data_state = 1;*/
        /*--------------------*/
      }
    }

    if ((PINB & 0x04) == 0)          //S3按鈕按下
    {
      if (in_db < 100)               //避免溢位
        in_db++;
      if (in_db == 1)                //按下10ms才算
      {
        key = 3;
        /*--單一按鈕顏色設定--*/
        led = 5;
        ledcolor[2]++;
        color = ledcolor[2];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---單一按鈕跑馬燈---*/
        /*
        runled = 6;
        runr = 0;
        rung = 0;
        runb = 255;
        mode = 2;
        data_state = 1;*/
        /*--------------------*/
      }
    }

    if ((PINB & 0x08) == 0)          //S4按鈕按下
    {
      if (in_db < 100)               //避免溢位
        in_db++;
      if (in_db == 1)                //按下10ms才算
      {
        key = 4;
        /*--單一按鈕顏色設定--*/
        led = 4;
        ledcolor[3]++;
        color = ledcolor[3];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---單一按鈕跑馬燈---*/
        /*
        runled = 7;
        runr = 255;
        rung = 255;
        runb = 0;
        mode = 2;
        data_state = 1;*/
        /*--------------------*/
      }
    }

    if ((PINB & 0x10) == 0)          //S5按鈕按下
    {
      if (in_db < 100)               //避免溢位
        in_db++;
      if (in_db == 1)                //按下10ms才算
      {
        key = 5;
        /*--單一按鈕顏色設定--*/
        led = 3;
        ledcolor[4]++;
        color = ledcolor[4];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---單一按鈕跑馬燈---*/
        /*
        runled = 7;
        runr = 0;
        rung = 255;
        runb = 255;
        mode = 2;
        data_state = 1;*/
        /*--------------------*/
      }
    }

    if ((PINB & 0x20) == 0)          //S6按鈕按下
    {
      if (in_db < 100)               //避免溢位
        in_db++;
      if (in_db == 1)                //按下10ms才算
      {
        key = 6;
        /*--單一按鈕顏色設定--*/
        led = 2;
        ledcolor[5]++;
        color = ledcolor[5];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---單一按鈕跑馬燈---*/
        /*
        runled = 7;
        runr = 255;
        rung = 0;
        runb = 255;
        mode = 2;
        data_state = 1;*/
        /*--------------------*/
      }
    }

    if ((PIND & 0x40) == 0)          //S7按鈕按下
    {
      if (in_db < 100)               //避免溢位
        in_db++;
      if (in_db == 1)                //按下10ms才算
      {
        key = 7;
        /*--單一按鈕顏色設定--*/
        led = 1;
        ledcolor[6]++;
        color = ledcolor[6];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---單一按鈕跑馬燈---*/
        /*
        runled = 7;
        runr = 255;
        rung = 255;
        runb = 255;
        mode = 2;
        data_state = 1;*/
        /*--------------------*/
      }
    }

    if ((PIND & 0x80) == 0)          //S8按鈕按下
    {
      if (in_db < 100)               //避免溢位
        in_db++;
      if (in_db == 1)                //按下10ms才算
      {
        key = 8;
        /*--單一按鈕顏色設定--*/
        led = 0;
        ledcolor[7]++;
        color = ledcolor[7];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---單一按鈕跑馬燈---*/
        /*
        runled = 7;
        runr = 96;
        rung = 96;
        runb = 96;
        mode = 2;
        data_state = 1;*/
        /*--------------------*/
      }
    }
    else
      in_db = 0;
  }
}

void color_select(void) //按鈕單一顏色選擇
{
  color = color % 9;
  _delay_ms(150);
}

void cleandata(void) //清除上一次顯示的全部暫存顏色
{
  int i;
  for (i = 0; i < 8; i++)
    setPixelColor(i, 0, 0, 0);
  for (i = 0; i < 8; i++)
    ledcolor[i] = 0;
  //  process_show();
  data_state = 0;
}

void button_color_set_function(void)//按鈕單一顏色功能
{
  setcolor();
  process_show();
}

void setcolor(void) //按鈕顏色設定
{
  if (data_state == 1)
    cleandata();
  switch (color)
  {
  case 1:
    setPixelColor(led, adc_to_color, 0, 0); //紅
    break;
  case 2:
    setPixelColor(led, 0, adc_to_color, 0); //綠
    break;
  case 3:
    setPixelColor(led, 0, 0, adc_to_color); //藍
    break;
  case 4:
    setPixelColor(led, adc_to_color, adc_to_color, 0); //黃
    break;
  case 5:
    setPixelColor(led, 0, adc_to_color, adc_to_color); //青
    break;
  case 6:
    setPixelColor(led, adc_to_color, 0, adc_to_color); //紫
    break;
  case 7:
    setPixelColor(led, adc_to_color, adc_to_color, adc_to_color); //白
    break;
  case 8:
    setPixelColor(led, 96, 96, 96); //灰
    break;
  case 0:
    setPixelColor(led, 0, 0, 0); //黑
    break;
  }
}

void button_runhorse_function(void) //按鈕跑馬燈功能
{
  setrunhorse();
  process_show();
  runhorse_old_led();
}


void setrunhorse(void) //跑馬燈設定
{
  if (data_state == 1)
    cleandata();
  switch (key)
  {
  case 1:
    setPixelColor(runled, runr, rung, runb); //紅
    break;
  case 2:
    setPixelColor(runled, runr, rung, runb); //綠
    break;
  case 3:
    setPixelColor(runled, runr, rung, runb); //藍
    break;
  case 4:
    setPixelColor(runled, runr, rung, runb); //黃
    setPixelColor(runled - 1, runr, rung, runb); //黃
    break;
  case 5:
    setPixelColor(runled, runr, rung, runb); //青
    break;
  case 6:
    setPixelColor(runled, runr, rung, runb); //紫
    setPixelColor(fabs(runled - 7), runr, rung, runb); //紫
    break;
  case 7:
    setPixelColor(fabs(runled), runr, rung, runb); //白
    break;
  case 8:
    setPixelColor(fabs(runled), runr, rung, runb); //灰
    break;
  }
}
void runhorse_old_led(void)//每執行一次跑馬燈模式都會進來此副程式(清除上次顏色並設定下一顆要亮的LED)
{
  switch (key)
  {
  case 1://右到左亮滅亮滅
    setPixelColor(runled, 0, 0, 0);
    usbreadnum = runled;
    runled--;
    if (runled == -1)
      runled = 7;
    break;
  case 2://右到左奇數亮滅
    setPixelColor(runled, 0, 0, 0);
    usbreadnum = runled;
    runled = runled - 2;
    if (runled == -1)
      runled = 7;
    break;
  case 3://右到左偶數亮滅
    setPixelColor(runled, 0, 0, 0);
    usbreadnum = runled;
    runled = runled - 2;
    if (runled == -2)
      runled = 6;
    break;
  case 4://右到左(12 23 34 45 56 67 78)
    setPixelColor(runled, 0, 0, 0);
    usbreadnum = runled;
    runled--;
    if (runled == 0) {
      runled = 7;
      setPixelColor(0, 0, 0, 0);
    }
    break;
  case 5://右到左逐一亮(不滅)
    usbreadnum = runled;
    runled--;
    if (runled == -1) {
      runled = 7;
      cleandata();
    }
    break;
  case 6://左右往內亮滅(18 27 36 45)
    setPixelColor(runled, 0, 0, 0);
    setPixelColor(fabs(runled - 7), 0, 0, 0);
    usbreadnum = runled;
    runled--;
    if (runled == 3)
      runled = 7;
    break;
  case 7://右到左亮滅後右到左亮滅
    setPixelColor(fabs(runled), 0, 0, 0);
    usbreadnum = fabs(runled);
    runled--;
    if (runled == -7)
      runled = 7;
    break;
  case 8://右到左亮滅後右到左亮滅
    setPixelColor(fabs(runled), 0, 0, 0);
    usbreadnum = fabs(runled);
    runled--;
    if (runled == -7)
      runled = 7;
    break;
  }
}

//顏色RGB
//紅(255,0,0)
//綠(0,255,0)
//藍(0,0,255)
//黃(255,255,0)
//青(0,255,255)
//紫(255,0,255)
//白(255,255,255)
//灰(128,128,128)//不明顯(偏白)
//灰(96,96,96)//較明顯(偏黑)
//黑(0,0,0)

//咖啡(255,96,0)
