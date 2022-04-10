#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(long n) asm("printInt");
void printBool(long n) asm("printBool");
void printString(char *n) asm("printString");
long count(char *n) asm("count");
void print6Ints(long n1, long n2, long n3, long n4, long n5, long n6) asm("print6Ints");
char *intToString(long n) asm("intToString");
char *readline() asm("readline");
char *tastyString() asm("tastyString");
long *makeOptional(long n) asm("makeOptional");
long *stringToOptInt(char *s) asm("stringToOptInt");
long *newTable(int numFields) asm("newTable");

const int MAX_STRING_LENGTH = 300;
const int MAX_DIGITS = 20;

void printInt(long n)
{
  printf("%ld\n", n);
}

void printBool(long n)
{
  if (n == 0)
    printf("false\n");
  else
    printf("true\n");
}

void print6Ints(long n1, long n2, long n3, long n4, long n5, long n6)
{
  printf("%ld %ld %ld %ld %ld %ld\n", n1, n2, n3, n4, n5, n6);
}

void printString(char *s)
{
  printf("%s\n", s);
}

long count(char *n) {
  return strlen(n);
}

char *append(char* s1, char* s2) {
  long s1Length = count(s1);
  long s2Length = count(s2);
  long newLength = s1Length + s2Length + 1;
  char *newString = malloc(newLength);
  strncpy(newString, s1, s1Length);
  strncpy(newString + s1Length, s2, s2Length);
  newString[newLength - 1] = '\0';
  return newString;
}

char *intToString(long n)
{
  char* newString = malloc(MAX_DIGITS + 1);
  sprintf(newString, "%ld", n);
  return newString;
}

char *readline() 
{
  char newString[MAX_STRING_LENGTH + 1];
  fgets(newString, MAX_STRING_LENGTH, stdin);
  size_t actualLength = strlen(newString);
  if (newString[actualLength - 1] == '\n') {
    newString[actualLength - 1] = '\0';
  }
  return tastyString(newString);
}

char *tastyString(char *s)
{
  size_t length = count(s);
  char *newString = malloc(length + 1);
  strncpy(newString, s, length);
  return newString;
}

long *makeOptional(long n) 
{
  long *newValue = malloc(8);
  *newValue = n;
  return newValue;
}

long *stringToOptInt(char *s)
{
  int n = count(s);

  // Convert the string to an integer
  // See: https://www.cplusplus.com/reference/cstdlib/strtol/
  char *e = NULL;
  long ans = strtol(s, &e, 10);

  // If the conversion did not succeed, return nil
  if (*e != '\0') {
    return NULL;
  }

  // Convert the integer to an optional
  return makeOptional(ans);
}

long* newTable(int numFields) {
  return malloc(8 * numFields);
}

extern void cs132start() asm("cs132start");

int main()
{
  cs132start();
}
