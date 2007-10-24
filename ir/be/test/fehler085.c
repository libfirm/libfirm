typedef struct TypeToken {
	unsigned int handle;
	unsigned short DbId;
	unsigned short CoreDbId;
} TokenType;

extern TokenType NullToken;
TokenType NullToken = {0, 0, 0};

int main(int argc, char **argv)
{
	TokenType t = NullToken;

	return t.handle;
}
