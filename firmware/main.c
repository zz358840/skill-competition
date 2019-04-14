//104�~�q���״_
//���}����
//PB0=S1,PB1=S2,,PB2=S3,PB3=S4,PB4=S5,PB5=S6
//PD6=S7,PD7=S8
//PD2=D+
//PD4=D-
//PC0=led8��Din
//PC5=�i�ܹq��
//��s���:10/26
//��s��x:�̫�榡�ƶ���{���X��VB�{���X
//�\�໡��:
//�T�ؼҦ��G�}���Ҧ� ���s�Ҧ� USB�Ҧ�

/*�q�q VB������*/
//�}���Ҧ��G�����Ŷ� �|���C�����{�{ ��������1��
//���s�Ҧ��G������@�C��P�]���O�\��
//��@�C��G�C�@�ӫ��s����]�w������led���C��
//�]���O�G�@�ӫ��s�N�@�Ӷ]���O�\��

/*�q�q VB����*/
//�|�@���P�BADC/�q��/key
//VB������ �O�l�|���^�}���Ҧ�
//�}���Ҧ��GVB�|�����P�B�O�l���C�� key=�ť�
//���s�Ҧ��GVB�|�����P�B�O�l���C��
//��@�C��GVB�|�����P�B�O�l���C��
//�]���O�GVB�|�����P�B�O�l���C��
//USB�Ҧ��G��VB��������O�l �]�i������@�C��P�]���O key=�ť�

/*
This example should run on most AVRs with only little changes. No special
hardware resources except INT0 are used. You may have to change usbconfig.h for
different I/O pins for USB. Please note that USB D+ must be the INT0 pin, or
at least be connected to INT0 as well.
*/
#include <avr/io.h>
#include <avr/wdt.h>
#include <avr/interrupt.h>  /* sei():�P�त�_ */
#include <util/delay.h>     /*  _delay_ms() */
#include <avr/eeprom.h>
#include <avr/pgmspace.h>   /* usbdrv.h�һݪ��ѼƩw�q */
#include "usbdrv.h"
#include "oddebug.h"        /* �ϥΰ����������d�� */
#include <math.h> 			/* �ƾǨ�� fabs():����� */

#define LED_NUM  8

/* �����ܼ� */
//page:�}���Ҧ������A���
//mode:�Ҧ����
//var1:��rainbow�ϥΪ��ܼ�
//renew:�t�Xmode�i�JUSB�Ҧ�
//temp_r, temp_g, temp_b:�m�i���h��rgb
//in_db:�P�_���s���קK���쪺�ܼ�
//key:���s�ܼ�
uchar page, mode, var1, renew , temp_r, temp_g, temp_b, in_db, key;

int data_state;  //data_state:�ثe�O�_�����(0�S�� 1��)

/* ��@�C�� �ܼư� */
//led:setPixelcolor��led(7=led1 6=led2 ... 0=led8)
//color:��@�C��]�w���ܼ�
//ledcolor[8]:�N�ثe��ܪ��C��ᵹcolor //led[0]:S1 led[1]:S2 ... led[7]:S8
int led, color, ledcolor[8];

/* �]���O �ܼư� */
//runled:�]���O��led�ܼ�
//runr,rung,runb:�]���O���C��
//usbreadnum:�brunled���U��e�N�{���ȶǵ�usbreadnum
int runled, runr, rung, runb, usbreadnum;

//usbwritenum:usb�g�Jled�s����x4��(�t�Xpixels_usb[32])
//pixels_usb[usbwritenum]:led
//pixels_usb[usbwritenum + 1]:r
//pixels_usb[usbwritenum + 2]:g
//pixels_usb[usbwritenum + 3]:b
int usbwritenum;

uchar *p;  //*p:��setPixelColor�禡�Ϊ������ܼ�

unsigned int timer[3];  //�X�����ɶ�(5ms*X)
uchar pixels[48];  // LED�Ϊ��}�C(�C��LED 3byte)
uchar pixels_usb[32];  //USB�Ϊ��}�C
static uchar AdcData[2] = {0,0};  //ADC�� L:AdcData[0] H:AdcData[1]
int adc_to_color=0; //�NADC����^�]�wRGB����(0~255)

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

void setAdc(void)  //�]�wADC
{
									//�q�D��� MUX3..0=0000-->ADC0,0001-->ADC1,0010-->ADC2
	ADMUX |= (0<<MUX3) | (1<<MUX2) | (0<<MUX1) | (1<<MUX0);	//0011-->ADC3,0100-->ADC4,0101-->ADC5,0110-->ADC6
									//0111-->ADC7,XX,1110-->1.23V,1111-->0V
	ADMUX |= (1<< REFS0) | (0<< REFS1); // �]�wADC�Ѧҹq�� REFS1 REFS0=00-->AREF,01-->AVCC,10-->X,11-->2.56V	
//	ADMUX |= (1 << ADLAR); // �]�w��X����覡 ADLAR=0-->�a�k,1-->�a��(8bit�ɥ�ADCH����)
															// �]�w ADC �w���W
	ADCSRA |= (1<< ADPS2) | (1<< ADPS1) | (1<< ADPS0); 	//ADPS2..0 =000�B001-->2,010-->4,011-->8
															//100-->16,101-->32,110-->64,111-->128
		
//	ADCSRA |= (1 << ADFR); // �]�w ADC �ഫ�Ҧ� ADFR=0-->�榸,1-->�s��
	ADCSRA |= (1 << ADEN); // ADC �P�� ADEN=0-->OFF,1--ON
}

void getAdc(void)  //��ADC��
{
	ADCSRA|=(1<<ADSC);					//Start ADC conversion ADSC=0-->����,1-->�Ұ�
//	while ((ADCSRA&_BV(ADIF))==0x00){} 	//Wait to finish conversion
	while (ADCSRA&_BV(ADSC)); 	//Wait to finish conversion
	AdcData[0]=ADCL;					//Ū�� ADC ����
	AdcData[1]=ADCH;
//	ADCSRA |= (1 << ADIF);				//�M�� ADIF
	adc_to_color= (256 * AdcData[1] + AdcData[0]) / 4.01; //���X�Ӫ�AD��0~1023 �ܼƬ���� ���H4��0~256 �]�����H�j��4�Y�i��0~255
}

/* ------------------------------ USB ���� --------------------------------- */
PROGMEM const char usbHidReportDescriptor[22] = {   /* USB ���i�y�z�� */
  0x06, 0x00, 0xff,              // USAGE_PAGE (�c��Ӧ۩w)
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

/* �H�U���ܼƬO�ΨӼȦs�ثe��ƶǿ骺���A */
static uchar    currentAddress;
static uchar    bytesRemaining;

/* usbFunctionRead():��D���n�qUSB�˸m�n�D�j���ƮɴN�|�I�susbFunctionRead() */
/* �����T���i�H�i�H�Ѧ�usbdrv/usbdrv.h��� */
uchar   usbFunctionRead(uchar *data, uchar len)//USBŪ�������
{
  if (len > bytesRemaining)
    len = bytesRemaining;
  if (currentAddress == 0)
  {
    switch (mode)
    {
      //��Ʈ榡
      //*(data)   = led;  //��@�C��:led �]���O:usbreadnum
      //*(data + 1) = p[0];  //��@�C��:p[0] �]���O:runr
      //*(data + 2) = p[1];  //��@�C��:p[1] �]���O:rung
      //*(data + 3) = p[2];  //��@�C��:p[2] �]���O:runb
      //*(data + 4) = mode;	  //�Ҧ�
      //*(data + 5) = key;  //���s
      //*(data + 6) = AdcData[0];  //ADC�C�q��
      //*(data + 7) = AdcData[1];  //ADC���q��
	  
    case 0:  //�}���Ҧ�
      *(data)   = 0;
      *(data + 1) = p[0];
      *(data + 2) = p[1];
      *(data + 3) = p[2];
      *(data + 4) = mode;	
      *(data + 5) = key;
      *(data + 6) = AdcData[0];
      *(data + 7) = AdcData[1];
      break;
    case 1:  //���s�Ҧ� ��@�C��
      *(data)   = led;
      *(data + 1) = p[0];
      *(data + 2) = p[1];
      *(data + 3) = p[2];
      *(data + 4) = mode;
      *(data + 5) = key;
      *(data + 6) = AdcData[0];
      *(data + 7) = AdcData[1];
      break;
    case 2:  //���s�Ҧ� �]���O
      *(data)   = usbreadnum;
      *(data + 1) = runr;
      *(data + 2) = rung;
      *(data + 3) = runb;	
      *(data + 4) = mode;
      *(data + 5) = key;
      *(data + 6) = AdcData[0];
      *(data + 7) = AdcData[1];
      break;
    case 3:  //USB�Ҧ�
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

/* usbFunctionWrite():��D���n�e�X�j���Ƶ�USB�˸m�ɴN�|�I�susbFunctionWrite()*/
/* �����T���i�H�i�H�Ѧ�usbdrv/usbdrv.h���*/
uchar   usbFunctionWrite(uchar *data, uchar len)//USB�g�J�����
{
  if (bytesRemaining == 0)
    return 1;               /* end of transfer */
  if (len > bytesRemaining)
    len = bytesRemaining;
  if (currentAddress == 0)
  {
    //���B�}�l�������
    //OutDataEightByte 0, 7, 255, 255, 255, 0, 0, 0
    //VB�g�J�P�O�lŪ���������Y
    //*(data + 0):�ť�(0)
    //*(data + 1):led�s��
    //*(data + 2):R
    //*(data + 3):G
    //*(data + 4):B
    //*(data + 5):�ť�(0)
    //*(data + 6):�ť�(0)
    //*(data + 7):usb�g�J�Ҧ����O�l �O�lŪ�������(�ȵL�]�w)

    if (*(data + 1) < LED_NUM)                    //*(data+1) led�s��
    {
      usbwritenum = *(data + 1) * 4;
      pixels_usb[*(data + 1) * 4]     = *(data + 1); //led�s��
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
  return bytesRemaining == 0; /* �p�G�o�O�̫�@���ơA�N�^��1 */
}

/* USB HID�s�խn�D: GET_REPORT�PSET_REPORT */
usbMsgLen_t usbFunctionSetup(uchar data[8])//USB��l�]�w
{
  usbRequest_t    *rq = (void *)data;
  if ((rq->bmRequestType & USBRQ_TYPE_MASK) == USBRQ_TYPE_CLASS) {  /* HID class request */
    if (rq->bRequest == USBRQ_HID_GET_REPORT) { /* wValue: ReportType (highbyte), ReportID (lowbyte) */
      /* �ѩ�Ȧ��@�ӳ��i�����A�]���������iID */
      bytesRemaining = 8;
      currentAddress = 0;
      return USB_NO_MSG;  /* �ϥ�usbFunctionRead()�Ӷǰe��ƨ�PC�D��*/
    } 
    else if (rq->bRequest == USBRQ_HID_SET_REPORT) {
      /* �ѩ�Ȧ��@�ӳ��i�����A�]���������iID */
      bytesRemaining = 8;
      currentAddress = 0;
      return USB_NO_MSG;  /* �ϥ� usbFunctionWrite()�qPC�D���������*/
    }
  } 
  else {
    /* �b���S���ϥγc��ӭn�D�A�]������ */
  }
  return 0;
}
/* ------------------------------------------------------------------------- */
ISR (TIMER0_OVF_vect)                       // timer0 overflow interrupt //���_���
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
  usbDeviceDisconnect();  /* �j��s�˸m�C�|�A����榹�Ƶ{���ɡA���_�O�Q���઺ */
  i = 0;
  while (--i)             /* ����USB���Y�ޥXPC�D���A���������ʧ@�A �ɶ� > 250 ms */
  {
    wdt_reset(); 		   /* ���m�ݪ����p�ɾ� */
    _delay_ms(1);
  }
  usbDeviceConnect();
  sei(); 		/* �P����餤�_ */

  TIMSK |= (1 << TOIE0);            //timer �]�w
  TCCR0 |= (1 << CS02);             // set prescaler to 256 and start the timer
  TCNT0 = 128;

  setAdc();

  /* �ܼƪ�l�]�w */
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
    wdt_reset(); 		/* ���m�ݪ����p�ɾ� */
    usbPoll(); 			/* ����USB����ǿ� */

    if (mode == 0) //�}���Ҧ�
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

    else if (mode == 1) //���s�Ҧ� ��@�C��
    {
      if (timer[0] == 0)                        //5ms x 2 = 10ms
      {
        timer[0] = 2;//4
        button_color_set_function();
      }
    }

    else if (mode == 2) //���s�Ҧ� �]���O
    {
      if (timer[0] == 0)                        //5ms x 199 = 995ms
      {
        timer[0] = 199;
        button_runhorse_function();
      }
    }

    else if ((mode == 3) && (renew)) //USB�Ҧ�
    {
      renew = 0;
      key = 0;
      setPixelColor(pixels_usb[usbwritenum], pixels_usb[usbwritenum + 1], pixels_usb[usbwritenum + 2], pixels_usb[usbwritenum + 3]);
      process_show();
    }

    if (timer[1] == 0) //10ms �ˬd�@������
    {
      timer[1] = 2;
      button_click();
    }

    if (timer[2] == 0) //25ms Ū��ADC
    {
      timer[2] = 5;
      getAdc();
    }

  }
  return 0;
}


/* ------------------------------------------------------------------------- */
void process_show(void)//setPixecolor�]�w��n�Φ��Ƶ{�������
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

void setPixelColor(uchar number, uchar color_r, uchar color_g, uchar color_b) //�]�w��@LED�C��
{
  if (number < LED_NUM)
  {
    p = &pixels[number * 3];
    p[0] = color_r;
    p[1] = color_g;
    p[2] = color_b;
  }
}

void colorWipe(uchar r, uchar g, uchar b, uchar wait) //�]�w������LED�C��
{
  uchar i;
  for (i = 0; i < LED_NUM; i++)
  {
    setPixelColor(i, r, g, b);
  }
  process_show();
  //   delay(wait);
}

void rainbow(uchar wait) //�m�i���h
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

void Wheel(uchar WheelPos) //��rainbow�Ϊ��C��Ȧs�]�w
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

void button_click(void) //�P�_���s
{
  if (((PINB & 0x01) == 0) && ((PINB & 0x02) == 0))           //S1�MS2���s�����U
  {
    if (in_db < 100)               	  //�קK����
      in_db++;
    if (in_db == 1)                	  //������U10ms�~��
    {
      mode = 0;
      cleandata();
    }
  }

  else
  {
    if ((PINB & 0x01) == 0)          //S1���s���U
    {
      if (in_db < 100)               //�קK����
        in_db++;
      if (in_db == 1)                //���U10ms�~��
      {
        key = 1;
        /*--��@���s�C��]�w--*/
        led = 7;
        ledcolor[0]++;
        color = ledcolor[0];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---��@���s�]���O---*/
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

    if ((PINB & 0x02) == 0)          //S2���s���U
    {
      if (in_db < 100)               //�קK����
        in_db++;
      if (in_db == 1)                //���U10ms�~��
      {
        key = 2;
        /*--��@���s�C��]�w--*/
        led = 6;
        ledcolor[1]++;
        color = ledcolor[1];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---��@���s�]���O---*/
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

    if ((PINB & 0x04) == 0)          //S3���s���U
    {
      if (in_db < 100)               //�קK����
        in_db++;
      if (in_db == 1)                //���U10ms�~��
      {
        key = 3;
        /*--��@���s�C��]�w--*/
        led = 5;
        ledcolor[2]++;
        color = ledcolor[2];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---��@���s�]���O---*/
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

    if ((PINB & 0x08) == 0)          //S4���s���U
    {
      if (in_db < 100)               //�קK����
        in_db++;
      if (in_db == 1)                //���U10ms�~��
      {
        key = 4;
        /*--��@���s�C��]�w--*/
        led = 4;
        ledcolor[3]++;
        color = ledcolor[3];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---��@���s�]���O---*/
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

    if ((PINB & 0x10) == 0)          //S5���s���U
    {
      if (in_db < 100)               //�קK����
        in_db++;
      if (in_db == 1)                //���U10ms�~��
      {
        key = 5;
        /*--��@���s�C��]�w--*/
        led = 3;
        ledcolor[4]++;
        color = ledcolor[4];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---��@���s�]���O---*/
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

    if ((PINB & 0x20) == 0)          //S6���s���U
    {
      if (in_db < 100)               //�קK����
        in_db++;
      if (in_db == 1)                //���U10ms�~��
      {
        key = 6;
        /*--��@���s�C��]�w--*/
        led = 2;
        ledcolor[5]++;
        color = ledcolor[5];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---��@���s�]���O---*/
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

    if ((PIND & 0x40) == 0)          //S7���s���U
    {
      if (in_db < 100)               //�קK����
        in_db++;
      if (in_db == 1)                //���U10ms�~��
      {
        key = 7;
        /*--��@���s�C��]�w--*/
        led = 1;
        ledcolor[6]++;
        color = ledcolor[6];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---��@���s�]���O---*/
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

    if ((PIND & 0x80) == 0)          //S8���s���U
    {
      if (in_db < 100)               //�קK����
        in_db++;
      if (in_db == 1)                //���U10ms�~��
      {
        key = 8;
        /*--��@���s�C��]�w--*/
        led = 0;
        ledcolor[7]++;
        color = ledcolor[7];
        color_select();
        mode = 1;
        /*--------------------*/
        /*---��@���s�]���O---*/
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

void color_select(void) //���s��@�C����
{
  color = color % 9;
  _delay_ms(150);
}

void cleandata(void) //�M���W�@����ܪ������Ȧs�C��
{
  int i;
  for (i = 0; i < 8; i++)
    setPixelColor(i, 0, 0, 0);
  for (i = 0; i < 8; i++)
    ledcolor[i] = 0;
  //  process_show();
  data_state = 0;
}

void button_color_set_function(void)//���s��@�C��\��
{
  setcolor();
  process_show();
}

void setcolor(void) //���s�C��]�w
{
  if (data_state == 1)
    cleandata();
  switch (color)
  {
  case 1:
    setPixelColor(led, adc_to_color, 0, 0); //��
    break;
  case 2:
    setPixelColor(led, 0, adc_to_color, 0); //��
    break;
  case 3:
    setPixelColor(led, 0, 0, adc_to_color); //��
    break;
  case 4:
    setPixelColor(led, adc_to_color, adc_to_color, 0); //��
    break;
  case 5:
    setPixelColor(led, 0, adc_to_color, adc_to_color); //�C
    break;
  case 6:
    setPixelColor(led, adc_to_color, 0, adc_to_color); //��
    break;
  case 7:
    setPixelColor(led, adc_to_color, adc_to_color, adc_to_color); //��
    break;
  case 8:
    setPixelColor(led, 96, 96, 96); //��
    break;
  case 0:
    setPixelColor(led, 0, 0, 0); //��
    break;
  }
}

void button_runhorse_function(void) //���s�]���O�\��
{
  setrunhorse();
  process_show();
  runhorse_old_led();
}


void setrunhorse(void) //�]���O�]�w
{
  if (data_state == 1)
    cleandata();
  switch (key)
  {
  case 1:
    setPixelColor(runled, runr, rung, runb); //��
    break;
  case 2:
    setPixelColor(runled, runr, rung, runb); //��
    break;
  case 3:
    setPixelColor(runled, runr, rung, runb); //��
    break;
  case 4:
    setPixelColor(runled, runr, rung, runb); //��
    setPixelColor(runled - 1, runr, rung, runb); //��
    break;
  case 5:
    setPixelColor(runled, runr, rung, runb); //�C
    break;
  case 6:
    setPixelColor(runled, runr, rung, runb); //��
    setPixelColor(fabs(runled - 7), runr, rung, runb); //��
    break;
  case 7:
    setPixelColor(fabs(runled), runr, rung, runb); //��
    break;
  case 8:
    setPixelColor(fabs(runled), runr, rung, runb); //��
    break;
  }
}
void runhorse_old_led(void)//�C����@���]���O�Ҧ����|�i�Ӧ��Ƶ{��(�M���W���C��ó]�w�U�@���n�G��LED)
{
  switch (key)
  {
  case 1://�k�쥪�G���G��
    setPixelColor(runled, 0, 0, 0);
    usbreadnum = runled;
    runled--;
    if (runled == -1)
      runled = 7;
    break;
  case 2://�k�쥪�_�ƫG��
    setPixelColor(runled, 0, 0, 0);
    usbreadnum = runled;
    runled = runled - 2;
    if (runled == -1)
      runled = 7;
    break;
  case 3://�k�쥪���ƫG��
    setPixelColor(runled, 0, 0, 0);
    usbreadnum = runled;
    runled = runled - 2;
    if (runled == -2)
      runled = 6;
    break;
  case 4://�k�쥪(12 23 34 45 56 67 78)
    setPixelColor(runled, 0, 0, 0);
    usbreadnum = runled;
    runled--;
    if (runled == 0) {
      runled = 7;
      setPixelColor(0, 0, 0, 0);
    }
    break;
  case 5://�k�쥪�v�@�G(����)
    usbreadnum = runled;
    runled--;
    if (runled == -1) {
      runled = 7;
      cleandata();
    }
    break;
  case 6://���k�����G��(18 27 36 45)
    setPixelColor(runled, 0, 0, 0);
    setPixelColor(fabs(runled - 7), 0, 0, 0);
    usbreadnum = runled;
    runled--;
    if (runled == 3)
      runled = 7;
    break;
  case 7://�k�쥪�G����k�쥪�G��
    setPixelColor(fabs(runled), 0, 0, 0);
    usbreadnum = fabs(runled);
    runled--;
    if (runled == -7)
      runled = 7;
    break;
  case 8://�k�쥪�G����k�쥪�G��
    setPixelColor(fabs(runled), 0, 0, 0);
    usbreadnum = fabs(runled);
    runled--;
    if (runled == -7)
      runled = 7;
    break;
  }
}

//�C��RGB
//��(255,0,0)
//��(0,255,0)
//��(0,0,255)
//��(255,255,0)
//�C(0,255,255)
//��(255,0,255)
//��(255,255,255)
//��(128,128,128)//������(����)
//��(96,96,96)//������(����)
//��(0,0,0)

//�@��(255,96,0)
