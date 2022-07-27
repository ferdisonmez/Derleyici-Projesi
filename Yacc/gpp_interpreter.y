%{
#include <stdio.h>
#include <string.h>
#include "gpp_interpreter.h"

void print_arr(int*);
int* concat_arr(int*, int*);
int* append_arr(int*, int);
int pow_func(int, int);

%}

%union{
    int ival;
    int *ivals;
    char id[20];
}

%start INPUT
%token STRING COMMENT OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_DBLMULT OP_OC OP_CC OP_COMMA KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS
KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_FOR KW_IF KW_EXIT
KW_LOAD KW_DISP KW_TRUE KW_FALSE NEWLINE


%token <ival> VALUE
%token <id> ID


%type <ival> INPUT
%type <ival> EXPI
%type <ival> EXPB
%type <ivals> VALUES
%type <ivals> EXPLISTI
%type <ivals> LISTVALUE

%%

INPUT: 
    EXPI {printf("SYNTAX OK. \nResult = %d\n", $1);}
    |
    EXPLISTI {printf("SYNTAX OK. \nResult = "); print_arr($1);}
    ;


EXPI:
    OP_OP OP_PLUS EXPI EXPI OP_CP  {$$=$3+$4;} /* (+ EXPI EXPI) */
    |
    OP_OP OP_MINUS EXPI EXPI OP_CP {$$=$3-$4;} /* (- EXPI EXPI) */
    |
    OP_OP OP_MULT EXPI EXPI OP_CP  {$$=$3*$4;} /* (* EXPI EXPI) */
    |
    OP_OP OP_DIV EXPI EXPI OP_CP   {$$=$3/$4;} /* (/ EXPI EXPI) */
    |
    OP_OP OP_DBLMULT EXPI EXPI OP_CP {$$ = pow_func($3, $4);}
    |
    ID {$$ = get_entry($1);}
    |
    VALUE {$$ = $1;}
    |
    OP_OP KW_SET ID EXPI OP_CP {$$ = $4; put_entry($3, $4);}/* (set Id EXPI) */
    |
    OP_OP KW_IF EXPB EXPI OP_CP {$$ = (1 == $3) ? $4: 0;} /* (if EXPB EXPI) */
    |
    OP_OP KW_FOR EXPB EXPI OP_CP { $$ = (1 == $3) ? $4 : 0; } /* (for EXPB EXPI)*/
    |
    OP_OP KW_IF EXPB EXPI EXPI OP_CP {$$ = (1 == $3) ? $4: $5;}
     /* (if EXPB EXPI EXPI) */
    |
    OP_OP KW_DISP EXPI OP_CP { $$ = $3; printf("Print: %d\n", $3);} 
    ;

EXPB:
    OP_OP KW_AND EXPB EXPB OP_CP {$$ = $3 && $4;}   /* (and EXPB EXPB) */
    |
    OP_OP KW_OR EXPB EXPB OP_CP  {$$ = $3 || $4;}    /* (or EXPB EXPB) */
    |
    OP_OP KW_NOT EXPB OP_CP  {$$ = ! ($3);}      /* (not EXPB) */
    |
    OP_OP KW_EQUAL EXPB EXPB OP_CP {$$ = ($3 == $4);}  /* (equal EXPB EXPB) */
    |
    OP_OP KW_EQUAL EXPI EXPI OP_CP {$$ = ($3 == $4);}  /* (equal EXPI EXPI) */
    |
    OP_OP KW_LESS EXPI EXPI OP_CP { $$ = $3 < $4; } /* (less EXPI EXPI) */
    |
    KW_TRUE  { $$ = 1; }   /* true */
    |
    KW_FALSE   { $$ = 0; } /* false */
    | 
    OP_OP KW_DISP EXPB OP_CP { $$ = $3; printf("Print: %s\n", ($3 ? "true":"false"));}
    ;

EXPLISTI:
    OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {$$ = concat_arr($3, $4);}
    |
    OP_OP KW_APPEND EXPI EXPLISTI OP_CP {$$ = append_arr($4, $3);}
    |
    OP_OP KW_LIST VALUES OP_CP {$$ = $3;}
    |
    LISTVALUE  {$$ = $1;}
    |
    OP_OP KW_DISP LISTVALUE OP_CP { $$ = $3; printf("Print: "); print_arr($3);}
    ;


LISTVALUE:  /* LISTVALUE -> ( VALUES ) | () | nil */
    OP_OP VALUES OP_CP {$$ = $2;}
    |
    OP_OP OP_CP { $$= NULL; }
    |
    KW_NIL { $$ = NULL;}
    ;

VALUES: 
    VALUES VALUE  {$$ = append_arr($1, $2);}
    |
    VALUE {$$ = NULL; $$ = append_arr($$, $1);}
    ;


%%

/*  error messages */
int yyerror(char *s) {
    fprintf(stderr, "SYNTAX ERROR. \n");
    return 0;
}

void print_arr(int *arr){

    printf("( ");
    for(int i=0;arr[i]!=-1; ++i)
        printf("%d ", arr[i]);
    printf(")\n");

}

int* append_arr(int *array, int num){ 
    if(array == NULL){ // create new arr.
        array = (int *)malloc(sizeof(int)*2);
        array[0] = num;
        array[1] = -1;
    } 
    else{ // expand the old arr.
        int *temp = array;
        int size = 0;

        while(*temp != -1){
            ++temp;
            ++size;
        }

        temp = array;
        array = (int*)(malloc(sizeof(int)*(size+2)));

        int i=0;
        for(i;i<size;++i)
            array[i] = temp[i]; // copy old array.
        array[i] = num; // add the item.
        array[i+1] = -1; // new size.
        free(temp);
    } 

    return array;     
}

int* concat_arr(int *arr1, int *arr2){
    int arr_size1=0, arr_size2=0;

    int *temp = arr1;
    while(*temp != -1){
        arr_size1++;
        temp++;
    }
    
    temp = arr2;
    while(*temp != -1){
        arr_size2++;
        temp++;
    }

    temp = (int *) malloc(sizeof(int) * (arr_size1 + arr_size2) + 2);
    int i=0;
    for(i;i<arr_size1;++i) // copy arr1.
        temp[i] = arr1[i];
    
    int j=0;
    for(j;j<arr_size2; ++j) // copy arr2.
        temp[i++] = arr2[j]; 

    temp[i] = -1;
    free(arr1);
    free(arr2);
    return temp;
}

int pow_func(int num, int pow) {
    if (pow != 0)
        return (num * pow_func(num, pow - 1));
    else
        return 1;
}

int main(int argc, char **argv)
{

    ++argv, --argc;
    start_table();


    for(;;){
        yyparse();
    }

    return 0;
}

