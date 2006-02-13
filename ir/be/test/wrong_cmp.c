#define MASK	0x00000020

int ctrl_space_write ()
{
	unsigned int  reg_offset = 160;

	if ((reg_offset & MASK) == MASK)
	{}

	return (0);
}

int main (void)
{
  return ctrl_space_write();
}
