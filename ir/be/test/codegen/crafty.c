int first_ones[256];

  int FirstOne(unsigned long long arg1)
  {
    union doub {
      unsigned short i[4];
      unsigned long long d;
    };
    union doub x;
    x.d=arg1;

    if (x.i[3])
      return (first_ones[x.i[3]]);
    if (x.i[2])
      return (first_ones[x.i[2]]+16);
    if (x.i[1])
      return (first_ones[x.i[1]]+32);
    if (x.i[0])
      return (first_ones[x.i[0]]+48);

    return(64);
  }
