#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char *loadFile(const char *fileName) {
    char *fileContent = calloc(60000, 1);
    FILE *f = fopen(fileName, "r");
    int c, i;
    for (i=0; (c=getc(f))!=EOF; ++i)
        fileContent[i]=(char)c;
    fileContent[i]='\0';
    return fileContent;
}

int reduce(char *s) {
    char *c1 = s, *c2 = s;
    int changed = 0;
    while (*c2 != '\0' && *(c2+1) != '\0') {
        if (*(c2+1) != *c2 && tolower(*(c2+1))==tolower(*c2)) {
            c2 += 2;
            changed = 1;
        }
        *(c1++) = *(c2++);
    }
    if (*c2 != '\0') *(c1++) = *(c2++);
    *c1 = '\0';
    return changed;
}

void copyWithout(char *c2, char *c1, char c) {
    char *sc1 = c1, *sc2 = c2;
    while (*c1) {
        while (*c1 != '\0' && tolower(*c1)==c) c1++;
        if (*c1=='\0') continue;
        *c2 = *c1;
        c2++; c1++;
    }
    *c2 = '\0';
}

int shortestPolymer(char * s) {
    int shortest = strlen(s)+1;
    char * cp = calloc(shortest, 1);
    for (char c='a'; c <= 'z'; ++c) {
        char *c1 = s, *c2 = cp;
        copyWithout(c2, c1, c);
        while (reduce(c2));
        if (strlen(c2) < shortest) {
            shortest = strlen(c2);
        }
    }
    free(cp);
    return shortest;
}

int main() {
    char * inp=loadFile("day05.txt");
    //char inp2[20] = "dabAcCaCBAcCcaDA";
    while (reduce(inp));
    printf("Solution for day 05, part 1: %d\n", strlen(inp));
    printf("Solution for day 05, part 1: %d\n", shortestPolymer(inp));
    free(inp);
}