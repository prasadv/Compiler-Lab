%{
   #include<stdio.h>
   #include<stdlib.h>
   #include<string.h>

   struct node
   {
    int type;
    char nodetype;
    char *name;
    int value;
    struct node *ptr1,*ptr2,*ptr3;
    struct Gsymbol* gentry;
    };

    struct Gsymbol
    {
     int type;
     char *name;
     int binding;
     int size;
     int *arr;
     struct Gsymbol* next;
     };

     struct node *root;
     struct Gsymbol *head=NULL,*tail=NULL;
     struct Gsymbol *lhead=NULL,*ltail=NULL;

void make_var(char *,int);
void make_arr(char *,int,int);
void make_lvar(char *,int);

struct node* makenode(int,char,char*,int,struct node*,struct node*,struct node*,struct Gsymbol*);
struct node* make_variable(char *);
struct node* make_array(char *,struct node*);

struct Gsymbol* lookup(char*);
struct Gsymbol* l_lookup(char*);
int m,n;
int calculate(struct node*);

%}
%union
     {
      struct node *ptr;
      int val;
      char *var;
      struct Gsymbol *variable;
      };

%left '+' '-'
%left '*' '/'
%left '>' '<' EQ NE GE LE
%left AND OR
%right NOT
%token <var> VARIABLE
%token <val> INTEGER
%token DECL ENDDECL INT BOOL BEG IF THEN ELSE ENDIF WHILE DO ENDWHILE WRITE READ END EQ GE LE NE MAIN OR NOT AND

%type <ptr> expr varii statement relexpr program statementlist 
%type <variable> vari


%%

program		: declaration INT MAIN '('')''{'l_declaration  BEG statementlist END '}'		{ calculate($9);
								 					return(0);	
													}
		  ;
declaration	: DECL declarationlist ENDDECL			
		  |
		  ;
declarationlist : declarationlist type vari';'		
		  |
		  ;
type		:INT						{m=0;}
		|BOOL						{m=1;}
		;
vari	 	 : vari ',' VARIABLE                              {  make_var($3,m); }
         	 | vari',' VARIABLE '['INTEGER']'                 { make_arr($3,$5,m); }
         	 | VARIABLE                                       {   make_var($1,m);}
         	 | VARIABLE '['INTEGER']'                         {  make_arr($1,$3,m); }
         	 ;

l_declaration	: DECL l_declarationlist ENDDECL
		 |
		 ;
l_declarationlist: l_declarationlist type1 vari1 ';'
		 |
		 ;
type1		 :INT						{n=0;}
		 | BOOL						{n=1;}
		  ;
vari1	 	 : vari1 ',' VARIABLE                              {  make_lvar($3,n); }
         	 | VARIABLE                                       {   make_lvar($1,n);}
         	 ;



statementlist	: statementlist statement';'			{$$=makenode(2,'s',"",0,$1,$2,NULL,NULL);}	
		  		|statement ';'			{$$=makenode(2,'s',"",0,$1,NULL,NULL,NULL);}
						  		
				;
statement	: varii'='expr					{if($1->gentry->type==0) $$=makenode(3,'=',"",0,$1,$3,NULL,NULL);
								else {printf("\nConflicting Types\n");exit(0);}}
		  |varii'='relexpr				{if($1->gentry->type==1) $$=makenode(3,'=',"",0,$1,$3,NULL,NULL);
		  						else {printf("\n Conflicting types\n");exit(0);}}
		  |READ'('varii')'				{$$=makenode(3,'r',"",0,$3,NULL,NULL,NULL);}
		  |WRITE'('expr')'				{$$=makenode(3,'w',"",0,$3,NULL,NULL,NULL);}
		  |IF'('relexpr')'THEN statementlist ELSE statementlist ENDIF 
		  								{
		  								 $$=makenode(3,'i',"",0,$3,$6,$8,NULL);
									
										 }
		  |IF'('relexpr')'THEN statementlist ENDIF			{
		  								 $$=makenode(3,'i',"",0,$3,$6,NULL,NULL);
												
										 }
		  |WHILE'('relexpr')'DO statementlist ENDWHILE			{
		  								$$=makenode(3,'d',"",0,$3,$6,NULL,NULL);
											
		  								}
		  
		  ;
expr		: INTEGER					{ $$=makenode(0,'x',"",$1,NULL,NULL,NULL,NULL); }
		  |expr'+'expr					{ $$=makenode(0,'+',"",0,$1,$3,NULL,NULL);}
		  |expr'-'expr					{ $$=makenode(0,'-',"",0,$1,$3,NULL,NULL);}
		  |expr'*'expr					{ $$=makenode(0,'*',"",0,$1,$3,NULL,NULL);}
		  |expr'/'expr					{ $$=makenode(0,'/',"",0,$1,$3,NULL,NULL);}
		  |varii					{ $$=$1;}
		  |'('expr')'					{ $$=$2;}
		  
   		  ;
relexpr		: expr'>'expr					{ $$=makenode(1,'>',"",0,$1,$3,NULL,NULL);}
		  |expr'<'expr					{ $$=makenode(1,'<',"",0,$1,$3,NULL,NULL);}
		  |expr EQ expr					{ $$=makenode(1,'e',"",0,$1,$3,NULL,NULL);}
		  |expr GE expr					{ $$=makenode(1,'g',"",0,$1,$3,NULL,NULL);}
		  |expr LE expr					{ $$=makenode(1,'l',"",0,$1,$3,NULL,NULL);}
		  |expr NE expr					{ $$=makenode(1,'n',"",0,$1,$3,NULL,NULL);}
		  |'('relexpr')'				{ $$=$2;}
		  |relexpr OR relexpr				{ $$=makenode(1,'o',"",0,$1,$3,NULL,NULL);}
		  |relexpr AND relexpr				{ $$=makenode(1,'&',"",0,$1,$3,NULL,NULL);}
		  |varii OR varii				{ if($1->type==$3->type && $1->type==1) $$=makenode(1,'o',"",0,$1,$3,NULL,NULL);else {printf("\n Only boolean variables allowed\n");exit(0);}}
		  |varii AND varii				{if($1->type==$3->type && $1->type==1) $$=makenode(1,'&',"",0,$1,$3,NULL,NULL);else {printf("\n Only boolean variables allowed\n");exit(0);}}
		  |NOT relexpr					{ $$=makenode(1,'~',"",0,$2,NULL,NULL,NULL);}
		  |NOT varii					{ if($2->type==1) $$=makenode(1,'~',"",0,$2,NULL,NULL,NULL);else {printf("\n Only boolean variables allowed\n");exit(0);}}

		  ;
varii		: VARIABLE					{ $$=make_variable($1);}
		  |VARIABLE'['expr']'				{  if($3->type==1) {printf("\nType Mismatch\n"); exit(0);} else $$=make_array($1,$3);}
		  ;

%%

int yyerror (char *msg) 
   {
 	return fprintf (stderr, "YACC: %s\n", msg);
   }

int main()
  {
    yyparse();
    return 0;
  }
  
void make_var(char *ch,int m)
{
 struct Gsymbol* temp;
temp=lookup(ch);
if(temp==NULL)
{ temp=malloc(sizeof(struct Gsymbol));
 temp->name = ch;
 temp->type=m;
 temp->size=1;
 temp->binding=0;
 temp->next=NULL;
 if(!head)
 	{
 	 head=temp;
 	 tail=temp;
 	 }
 else	tail->next=temp;
 tail=temp;
}else
	{ printf("\nAlready declared\n");
	  exit(0);
	}
 }

void make_lvar(char *ch,int m)
{
 struct Gsymbol* temp;
temp=l_lookup(ch);
if(temp==NULL)
{temp=malloc(sizeof(struct Gsymbol));
 temp->name = ch;
 temp->type=m;
 temp->size=1;
 temp->binding=0;
 temp->next=NULL;
 if(!lhead)
 	{
 	 lhead=temp;
 	 ltail=temp;
 	 }
 else	ltail->next=temp;
 ltail=temp;
}
else{ printf("\nAlready declared\n");
	  exit(0);
	}
}
 void make_arr(char *ch,int a,int m)
 {
  struct Gsymbol* temp;
  temp=lookup(ch);
  if(temp==NULL)
{ temp=malloc(sizeof(struct Gsymbol));
  temp->name = ch;
  temp->type=m;
  temp->size=a;
  temp->binding=0;
  temp->arr=(int *)malloc(a*4);
  temp->next=NULL;
  if(!head)
 	{
 	 head=temp;
 	 tail=temp;
 	 }
 else	tail->next=temp;
 tail=temp;
 }
else{ printf("\nAlready declared\n");
	  exit(0);
	}
}


 struct node* makenode(int type, char nodetype,char* name,int value,struct node* ptr1,struct node* ptr2,struct node* ptr3,struct Gsymbol* gentry)
 {
  struct node *temp=(struct node*)malloc(sizeof(struct node));
  temp->type=type;
  temp->nodetype=nodetype;
  temp->name=name;
  temp->value=value;
  temp->ptr1=ptr1;
  temp->ptr2=ptr2;
  temp->ptr3=ptr3;
  temp->gentry=gentry;
	

return temp;
 }

 struct node* make_variable(char *name)
 {
	struct Gsymbol* temp ;
        temp = l_lookup(name); 
	
  if (temp!=NULL)
  {
  		return (makenode(temp->type, 'v', name, 0, NULL, NULL, NULL, temp));
 }  
  else 
    {
	temp = lookup(name);
	if(temp!=NULL){
		return (makenode(temp->type, 'v', name, 0, NULL, NULL, NULL, temp));
		}
     }
       
	
        printf("\nVariable not declared\n");
        exit(0);
       
 }

  struct node* make_array(char *name,struct node* value)
 {
   struct Gsymbol* temp = l_lookup(name);
   if (temp!=NULL)
   		return (makenode(temp->type, 'a', name, 0, value, NULL, NULL, temp));

   else
   {
	temp=lookup(name);
	if(temp!=NULL)
		return (makenode(temp->type, 'a', name, 0, value, NULL, NULL, temp));
    }
  
        printf("\nVariable not declared\n");
        exit(0);
     
}
 struct Gsymbol* lookup(char *a)
 {
   struct Gsymbol* temp;
   temp=head;
   while(temp!=NULL)
   {
    if(strcmp(temp->name,a)==0)
    break;
    else  temp=temp->next;
   }	
   return temp;
 }

struct Gsymbol* l_lookup(char *a)
 {
   struct Gsymbol* temp;
   temp=lhead;
   while(temp!=NULL)
   {
    if(strcmp(temp->name,a)==0)
    break;
    else  temp=temp->next;
   }	
   return temp;
 }
  int calculate(struct node* a)
  {
	if(a==NULL)
		return 0;


   int t;

   if(a->type==0||a->type==1)
          {
            if(a->nodetype=='a')
             { t=calculate(a->ptr1);
             return (a->gentry->arr[t]);
             }

             else if(a->nodetype=='v') return(a->gentry->binding);
           }
	
	if(a->type==2)	{calculate(a->ptr1);
			calculate(a->ptr2);			
			}

    else
          {
           switch(a->nodetype)
             {
				case '+' : return(calculate(a->ptr1)+calculate(a->ptr2));
						   break;
				case '-' : return(calculate(a->ptr1)-calculate(a->ptr2));
						   break;
				case '*' : return(calculate(a->ptr1)*calculate(a->ptr2));
						   break;
				case '/' : return(calculate(a->ptr1)/calculate(a->ptr2));
						   break;
				case '>' : if (calculate(a->ptr1)>calculate(a->ptr2))
							return 1;
							else return 0;
							break;
				case '<' : if(calculate(a->ptr1)<calculate(a->ptr2))
							return 1;
							else return 0;
							break;
				case 'e' : if(calculate(a->ptr1)==calculate(a->ptr2))
							return 1;
							else return 0;
							break;
				case 'g' : if(calculate(a->ptr1)>=calculate(a->ptr2))
							return 1;
							else return 0;
							break;
				case 'l' : if(calculate(a->ptr1)<=calculate(a->ptr2))
							return 1;
							else return 0;
							break;
				case 'n' : if(calculate(a->ptr1)!=calculate(a->ptr2))
							return 1;
							else return 0;
							break;
				case '=' : if(a->ptr1->nodetype=='v'){
							a->ptr1->gentry->binding=calculate(a->ptr2);
						   	
						   }
						   else
						     if(a->ptr1->nodetype=='a')
						     a->ptr1->gentry->arr[calculate(a->ptr1->ptr1)]=calculate(a->ptr2);
							break;
				case 'r' : printf("\nEnter the value\n");scanf("%d",&t);
						   if(a->ptr1->nodetype=='v')
						    a->ptr1->gentry->binding=t;
						   else if(a->ptr1->nodetype=='a')
						   	a->ptr1->gentry->arr[calculate(a->ptr1->ptr1)]=t;
						   	break;
				case 'w' : printf("\nValue %d\n",calculate(a->ptr1));
						   break;

				case 'i' : if(a->ptr3!=NULL)
							{
								if(calculate(a->ptr1)==1)
									calculate(a->ptr2);
								else calculate(a->ptr3);
							 }
							else {
									if(calculate(a->ptr1)==1)
							 		calculate(a->ptr2);
							    	else ;
							      }
							 break;
				 case 'd' : while(calculate(a->ptr1)==1){
						calculate(a->ptr2);}
							 break;
				case 'x' : return(a->value); break;
				case 'o' :return(calculate(a->ptr1)||calculate(a->ptr2));break;
				case '&' :return(calculate(a->ptr1)&&calculate(a->ptr2));break;
				case '~' :return(!calculate(a->ptr1));break;

			}
	}
		
}  
