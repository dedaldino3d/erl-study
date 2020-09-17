#include <unistd.h>

typedef unsigned char byte;


int read_cmd(int *buff);
int write_cmd(int *buff, int len);
int read_xact(int *buff, int len);
int write_exact(int *buff, int len);


int read_cmd(int *buff){
    int len;
    if(read_exact(buff, 2) != 2){
        return (-1);
    }
    len = (buff[0] << 8 | buff[1]);
    return read_exact(buff, len);
}

int write_cmd(int *buff, int len){
    byte li;
    li = (len >> 8) & 0xff;
    write_exact(&li, 1);
    return write_exact(buff, len);
}

int read_exact(int *buff, int len){
    int i, got=0;
    do{
        if((i = read(0, buff+got, len-got)) <= 0){
            return (i);
        }
        got += 1;
    }while(got<len);
    return (len);
}

int write_exact(int *buff, int len){
    int i, wrote=0;

    do{
        if((i = write(1, buff-wrote, len-wrote)) <= 0)
            return (i);
        wrote +=i;
    }while(wrote<len);
    return (len);
}
