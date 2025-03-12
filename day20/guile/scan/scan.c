#include <stdio.h>
#include <stdlib.h>

static char *buf;
static char *buf2;

void sanity();


int at(int x, int y){
  if (x < 0 || x >= 96){
    return 0;
  }
  if (y < 0 || y >= 96){
    return 0;
  }
  // # hash 
  return 35 == buf[x + (y * 100)] ;  
}

void rotate (){
  sanity();
  int i , j ;
  // rotate onto buffer 2 
  for (j=0; j <96; j++){
    for (i=0; i< 96; i++){
      buf2[(95-j) + (i * 100)] = buf[i + (j*100)];
    }// for i    
  }// for j

  // back-copy buffer2 onto buf 1
  for (j=0; j <96; j++){
    for (i=0; i< 96; i++){
      buf[i + (j * 100)] = buf2[i + (j*100)];
    }
  }
  sanity();
  
}

void flip (){
  sanity();
  int i , j ;
  // rotate onto buffer 2 
  for (j=0; j <96; j++){
    for (i=0; i< 96; i++){
      buf2[(95-i) + (j * 100)] = buf[i + (j*100)];
    }// for i    
  }// for j

  // back-copy buffer2 onto buf 1
  for (j=0; j <96; j++){
    for (i=0; i< 96; i++){
      buf[i + (j * 100)] = buf2[i + (j*100)];
    }
  }
  sanity();
  
}



/*
rotate
rotate
rotate
rotate
flip-horz
rotate
rotate
rotate
rotate ... should find nessy somewhere
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
 */



int ness (int x, int y){
  //  printf("checking ...%d %d ....\n" ,x,y);
  if (at(x + 18,y + 0) &&

      at(x + 0,y + 1) &&
      at(x + 5,y + 1) &&
      at(x + 6,y + 1) &&
      at(x + 11,y + 1) &&
      at(x + 12,y + 1) &&
      at(x + 17,y + 1) &&
      at(x + 18,y + 1) &&
      at(x + 19,y + 1) &&
      
      at(x + 1,y + 2) &&
      at(x + 4,y + 2) &&
      at(x + 7,y + 2) &&
      at(x + 10,y + 2) &&
      at(x + 13,y + 2) &&
      at(x + 16,y + 2) 
      ){

    printf("found nessy at %d %d \n" ,x,y);
    return 1;
  }
  return 0;
}


// 0..95 ok 96 97 98 99 should be empty
void sanity (){
  int i , j ;
  for (j=0; j < 96; j++){
    for (i=96; i< 100; i++){
      if (buf[i + j *100] != ' '){
	printf("failed sanity at %d %d \n" ,i ,j );
	exit(3);
      }
    }// for i
  }// for j
  printf("pass sanity check\n");  
}



void look (){
  sanity();
  
  int i , j ;
  for (j=0; j < 96; j++){
    for (i=0; i< 96; i++){
      ness(i,j);
    }// for i
  }// for j
  sanity();
  
}


int main(){

  FILE *fp = fopen("../lochness.txt","r");
  //FILE *fp = fopen("../ness.txt","r");
  //FILE *fp = fopen("../example-loch.txt","r");
  if(!fp){
    fprintf(stderr,"cannot open ness.txt");
    exit(2);
  }
  // make a 100 x 100 buffer
  buf = (char *)malloc(sizeof(char) * 102 * 102);
  if (!buf){
    fprintf(stderr,"cannot malloc buffer");
    exit(2);
  }
  buf2 = (char *)malloc(sizeof(char) * 102 * 102);
  if (!buf2){
    fprintf(stderr,"cannot malloc buffer2");
    exit(2);
  }

  
  int wid = 101;
  int hgt = 101;
  int end = wid * hgt;
  buf[end] = 0;
  buf2[end] = 0;
  
  int i = 0 , j = 0;
  for(i =0; i < end ; i++){
    buf[i] = ' ';
    buf2[i] = ' ';    
  }

  
  // 
  i = 0;
  j = 0;
  while(!feof(fp)){
    char ch = fgetc(fp);
    //printf("%c",ch);
    
    if (ch =='\n'){
      j = j + 100;
      i = j;
    }
    else { //if (ch == '#' || ch == '.'){
      buf[i] = ch;
      i++;
    }    
  }		     
  fclose(fp);

  /*
  for(i =0; i < 100 ; i++){
    printf("line %d : %s\n" ,i , buf[i]);
  }
  */


  FILE *fp2 = fopen("out.txt","w");
  if(!fp2){
    fprintf(stderr,"cannot open ness.txt");
    exit(2);
  }
  
  for (j=0; j <96; j++){
    for (i=0; i< 96; i++){
      fprintf(fp2,"%c",buf[i + j*100]);
      printf("%c",buf[i + j*100]);
    }// for i
    fprintf(fp2,"\n");
    printf("\n");    
  }// for j

  fclose(fp2);

  printf("\nThe search for nessy begins ...\n");    

  // lots of looking
  look(); rotate();
  look(); rotate();
  look(); rotate();
  look(); rotate();
  look();  flip();
  look(); rotate();
  look(); rotate();
  look(); rotate();
  look(); rotate();
  look();
  
    
  /* printf("\n%s",&buf[0]); */
  printf("\nreached here ok..\n");
  
  return 0;
}

	  
