#ifdef __GNUC__
unsigned int gnu_dev_major (unsigned long long int __dev)
{
  return ((__dev >> 8) & 0xfff) | ((unsigned int) (__dev >> 32) & ~0xfff);
}

unsigned int gnu_dev_minor (unsigned long long int __dev)
{
  return (__dev & 0xff) | ((unsigned int) (__dev >> 12) & ~0xff);
}

unsigned long long int gnu_dev_makedev (unsigned int __major, unsigned int __minor)
{
  return ((__minor & 0xff) | ((__major & 0xfff) << 8)
      | (((unsigned long long int) (__minor & ~0xff)) << 12)
      | (((unsigned long long int) (__major & ~0xfff)) << 32));
}
#endif

int main()
{
    return 0;
}
