/* Xgprintf --- extended formatted output via generic printer functions.
   Copyright (C) 1995, 1996 Christian von Roques */

/* $Id$ */

/* Implements ANSI-C printf formats minus locale plus extensions,
   noteably GNU Libc-like registering of specifier-handlers.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifndef USE_PRINTF

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include "misc.h"
#include "xprintf.h"


/* >= size of ASCII representation of a number using base 8 + 1 */
#define BUFSIZE ((sizeof(unsigned long)*CHAR_BIT)/3 + 2)

/* For printing double/float numbers, must be large enough for
   arbitrary numbers with %g.  @@@ Yeek! */
#define FBUFSIZE 128

static xprintf_function *print_func[UCHAR_MAX+1];


void
xprintf_register (char spec, xprintf_function func)
{
  assert ((spec > 0) && !print_func[(unsigned char)spec]);
  print_func[(unsigned char)spec] = func;
}

int
xvgprintf (xgprintf_func *out, void *arg, const char *fmt, va_list args)
{
  int done;		/* number of chars printed */
  unsigned long num;
  int is_neg;
  long signed_num;
  int base;
  char c;
  char fbuf[FBUFSIZE];	/* buffer for sprintf @@@ Yeek! */
  char buf[BUFSIZE];	/* buffer for itoa */
  char *str;		/* buffer pointer for number conversion */
  const char *s;	/* string to be printed by string: */
  int len;		/* length of s */
  char pad;		/* padding: ' ' or '0' */
  int showsign;		/* always show sign ['+'] */
  int space;		/* print space if positive */
  int left;		/* left justify */
  int alt;		/* alternate format 0x... */
  char fc;		/* conversion specifier */
  int width;		/* width of output field */
  int prec;		/* min. # of digits for integers; max
			   number of chars for from string */
  int qualifier;	/* 'h', 'l', or 'L' for integer fields */

  done = 0;

#define P(__b,__l) do { out (arg, __b, __l); done += __l; } while (0)

  while (*fmt) {
    const char *next = strchr (fmt, '%');

    if (!next) {
      P (fmt, strlen(fmt));
      break;
    } else if (next != fmt) {
      P (fmt, next-fmt);
      fmt = next;
    }

    /* Check for "%%".  Note that although the ANSI standard lists
       '%' as a conversion specifier, it says "The complete format
       specification shall be `%%'," so we can avoid all the width
       and prec processing.  */
    if (fmt[1] == '%') {
      P (fmt, 1);
      fmt += 2;
      continue;
    }

    /* process flags */
    is_neg = showsign = space = left = alt = 0;  pad = ' ';

  repeat:
    ++fmt;			/* this also skips first '%' */
    switch (*fmt) {
    case '-': left = 1; goto repeat;
    case '+': showsign = 1; goto repeat;
    case ' ': space = 1; goto repeat;
    case '#': alt = 1; goto repeat;
    case '0': pad = '0'; goto repeat;
    }

    /* get field width */
    width = 0;
    if (*fmt == '*') {
      ++fmt, width = va_arg(args, int);
      if (width < 0) left = 1, width = -width;
    } else while (isdigit(*fmt)) width = 10*width + *fmt++ - '0';

    /* get the prec */
    if (*fmt == '.') {
      ++fmt;
      if (*fmt == '*') ++fmt, prec = va_arg(args, int);
      else { prec = 0; while (isdigit(*fmt)) prec = 10*prec + *fmt++ - '0'; }
      if (prec < 0) prec = 0;
    } else prec = -1;		/* -1 == unspecified */

    /* get the conversion qualifier */
    if (*fmt == 'h' || *fmt == 'l' || *fmt == 'L')
      qualifier = *fmt++;
    else
      qualifier = -1;

    fc = *fmt++;

    if (print_func[(unsigned char)fc]) {
      xprintf_info info;
      int func_done;

      info.prec = prec;
      info.width = width;
      info.spec = fc;
      info.is_long_double = qualifier == 'L';
      info.is_short       = qualifier == 'h';
      info.is_long        = qualifier == 'l';
      info.alt = alt;
      info.space = space;
      info.left = left;
      info.showsign = showsign;
      info.pad = pad;

      /* Sharing `args' with another function is not blessed by ANSI
	 C.  From ISO/IEC DIS 9899, section 4.4:

	 If access to the varying arguments is desired, the called
	 function shall declare an object (referred as `ap' in this
	 section) having type va_list.  The object `ap' may be passed
	 as an argument to another function; if that function invokes
	 the va_arg macro with parameter `ap', the value of `ap' in
	 the calling function is indeterminate and shall be passed to
	 the va_end macro prior to any futher reference to `ap'.

	 Nevertheless, it works with most compilers, including gcc.  */
      func_done = print_func[(unsigned char)fc] (out, arg, &info, &args);
      if (func_done < 0) return -1;
      else done += func_done;

    } else {

      /* default base */
      base = 10;

      switch (fc) {

      case 'c':			/* Character */
		{ c = (char) va_arg (args, int);
		s = &c;
		len = 1;
		goto string;
		}

      case 's':			/* String */
		{ static const char null[] = "(null)";
		s = va_arg(args, char *);
		if (!s) {
		  s = null;
		  len = (prec == -1 || prec >= (int) sizeof(null) - 1) ? sizeof(null) - 1 : 0;
		} else {
		  len = strlen (s);
		}

		string:
		if (prec >= 0 && prec < len)
		  len = prec;
		width -= len;

		if (!left)
		  while (width-- > 0)
			P (" ", 1);
		P (s, len);
		while (width-- > 0)
		  P (" ", 1);
		break;
		}
      case 'p':			/* Pointer */
		{	const char nil[] = "(nil)";
		const void *ptr = va_arg (args, void *);
		if (!ptr && (prec==-1 || prec>=(int)sizeof(nil)-1)) {
		  s = nil;
		  len = sizeof(nil) - 1;
		  goto string;
		}

		base = 16; alt = 1; fc = 'x';
		num = (unsigned long) ptr;
		goto number2;
		}

      case 'o':			/* Octal */
		base = 8;
      case 'u':			/* Unsigned */
		goto number;

      case 'X':			/* heXadecimal */
      case 'x':			/* heXadecimal */
		base = 16;
      number:			/* get and print a unsigned number */

		if (qualifier == 'l')
		  num = va_arg(args, unsigned long);
		else if (qualifier == 'h')
		  /* vormals unsigned short, falsch fuer gcc 2.96
		     siehe http://mail.gnu.org/pipermail/discuss-gnustep/1999-October/010624.html */
		  num = va_arg(args, unsigned int);
		else
		  num = va_arg(args, unsigned int);
		/* ANSI only specifies the `+' and ` ' flags for signed conversions.  */
		is_neg = showsign = space = 0;
		goto number2;

      case 'd':			/* Decimal */
      case 'i':			/* Integer */
		if (qualifier == 'l')
		  signed_num = va_arg(args, long);
		else if (qualifier == 'h')
		  /* vormals short, falsch fuer gcc 2.96 siehe
		     http://mail.gnu.org/pipermail/discuss-gnustep/1999-October/010624.html */
		  signed_num = va_arg(args, int);
		else
		  signed_num = va_arg(args, int);
		num = (is_neg = signed_num < 0) ? - signed_num : signed_num;

      number2:			/* print number in num */
		{
		  static const char conv_TABLE[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		  static const char conv_table[] = "0123456789abcdefghijklmnopqrstuvwxyz";
		  const char *conv = fc=='X' ? conv_TABLE : conv_table;
		  str = buf+BUFSIZE;	/* str = _itoa(buf+BUFSIZE,num,base,fc=='X') */
		  if (!num) *--str = '0';
		  else do *--str = conv[num % base]; while (num/=base);
		  len = buf + BUFSIZE - str;
		}

		/* prepend 0 for octal format. */
		if (alt && base == 8 && prec<=len) {
		  *--str = '0';
		  ++len;
		}

		/* we will print s ==> cut prec and width */
		prec -= len;
		width -= len;

		if (prec > 0) width -= prec; /* we will use all precision space! */

		if (is_neg || showsign || space) --width;
		if (base == 16 && alt) width -= 2;

		if (!left && pad==' ')
		  while (width-- > 0) P (" ", 1);

		if (is_neg)
		  P ("-", 1);
		else if (showsign)
		  P ("+", 1);
		else if (space)
		  P (" ", 1);

		if (base == 16 && alt) {
		  P ("0", 1);
		  P (&fc, 1);
		}

		if (!left && pad=='0')
		  while (width-- > 0) P ("0", 1);

		while (prec-- > 0) P ("0", 1);

		P (str, len);

		while (width-- > 0)  P (" ", 1);
		break;

		/* @@@ NYI (just hacked) */
      case 'e':
      case 'E':
      case 'f':
      case 'g':
      case 'G':
#ifdef HAVE_ANSI_SPRINTF
		len = sprintf (fbuf, "%1.20e", va_arg (args, double));
#else
		sprintf (fbuf, "%1.20e", va_arg (args, double));
		len = strlen (fbuf);
#endif
		s = fbuf;
		goto string;

      case 'n':			/* assign #printed characters */
		if (qualifier == 'l')		*va_arg (args, long *) = done;
		else if (qualifier == 'h')	*va_arg (args, short *) = done;
		else { assert (qualifier == -1); *va_arg (args, int *) = done; }
		break;

      case 'm':			/* errno, GNU extension */
		/* strerror() is ANSI C, sys_nerr & sys_errlist are not */
		s = strerror (errno);
		len = strlen (s);
		goto string;

      default:
		assert (0);
      }
    }
  }
  return done;
}


int
xgprintf (xgprintf_func *out, void *arg, const char *fmt, ...)
{
  va_list args;
  int i;

  va_start (args, fmt);
  i = xvgprintf (out, arg, fmt, args);
  va_end (args);
  return i;
}

#endif /* !USE_PRINTF */
