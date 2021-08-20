/*
 * Simple interpreter of s-expressions
 */
#include <errno.h>
#include <stdio.h>
#include <sexp.h>
#include <sexp_ops.h>
#include <string.h>

#include "dune_parser.h"

#if LOCAL_INTERFACE
typedef struct dict {
  char varname[255];
  sexp_t *valexp;
  struct dict *next;
} dict_t;
#endif

/**
 * lookup a variable.  if we find it, return it.  Otherwise
 * return null.
 */
sexp_t *lookup(char *varname, dict_t *d) {
  sexp_t *ret = NULL;
  dict_t *_d = d;

  printf("Lookup : %s\n",varname);

  while (_d != NULL) {
    if (strcmp(varname,_d->varname) == 0) {
      ret = _d->valexp;
      break;
    }
    _d = _d->next;
  }

  return ret;
}

/**
 * insert a variable into the dictionary
 */
dict_t *insert(char *varname, sexp_t *val, dict_t *d) {
  dict_t *_d = d;
  char dbgbuf[BUFSIZ];

  printf("Inserting %s into dictionary.\n",varname);
  print_sexp(dbgbuf,BUFSIZ,val);
  printf("%s value is : %s\n",varname,dbgbuf);

  if (_d == NULL) {
    /* empty dictionary -- create one entry and return */
    _d = (dict_t *)malloc(sizeof(dict_t));
    _d->valexp = val;
    strcpy(_d->varname,varname);
    _d->next = NULL;
  } else {
    /* not empty, so first see if the name is already used.
       If so, purge the expression that was there and replace it. */
    while (1) {
      if (strcmp(_d->varname,varname) == 0) {
        destroy_sexp(_d->valexp);
        _d->valexp = val;
        return d;
      }

      /* if we're at the end with nothing left, break out of the loop */
      if (_d->next == NULL) break;
      _d = _d->next;
    }

    /* we're at the end, so tack on one entry for the value we want
       to add. */
    _d->next = (dict_t *)malloc(sizeof(dict_t));
    _d->next->valexp = val;
    strcpy(_d->next->varname,varname);
    _d->next->next = NULL;
    return d;
  }

  return _d;
}

/**
 * look up an entry and purge it -- not done yet
 */
dict_t *purge(char *varname, dict_t *d) {
  /* find an entry and purge it */
  return d;
}

/**
 * purge all entries in the dictionary and free the dictionary itself.
 */
void purge_all(dict_t *d) {
  dict_t *_d = d;
  dict_t *td;

  while (_d != NULL) {
    td = _d->next;
    printf("PURGING: %s\n",_d->varname);
    destroy_sexp(_d->valexp);
    free(_d);
    _d = td;
  }
}


/******************
   eval function
*******************/
dict_t *eval(sexp_t *exp, dict_t *env) {
    printf("eval\n");
    char *v;
    dict_t *d = env;
    sexp_t *tmpsx, *tmpsx2;

    /**
     * values understood here:
     *   library
     *   name
     *   public_name
     *   flags
     *   libraries
     *   coq.pp
     */
    /* printf("sexp type: */
    if (exp->ty == SEXP_LIST) {
        if (exp->list->ty == SEXP_VALUE)
            v = exp->list->val;
        else return env;
    } else return env;
    printf("v: %s\n", v);

    if (strcmp(v,"library") == 0) {
        /* d = insert(exp->list->next->val,exp->list->next->next,env); */
        /* exp->list->next->next = NULL; */
    } else if (strcmp(v,"name") == 0) {
        printf("NAME: %s\n",exp->list->next->val);
               /* exp->list->next->next->val); */
        /* tmpsx = lookup(exp->list->next->val,d); */
        /* d = eval(tmpsx,d); */
        /* tmpsx = lookup(exp->list->next->next->val,d); */
        /* d = eval(tmpsx,d); */
    } else if (strcmp(v,"public_name") == 0) {
        printf("PUBLIC_NAME: %s\n",exp->list->next->val);
        /* printf("PUBLIC_NAME AT: %s,%s\n",exp->list->next->val, */
        /*        exp->list->next->next->val); */
    } else if (strcmp(v,"flags") == 0) {
        printf("FLAGS: ");
        tmpsx = exp->list->next;
        while (tmpsx != NULL) {
            printf(" %s |", tmpsx->val);
            /* if (tmpsx->ty == SEXP_VALUE) { */
            /*     tmpsx2 = lookup(tmpsx->val,d); */
            /*     d = eval(tmpsx2,d); */
            /* } else { */
            /*     d = eval(tmpsx,d); */
            /* } */
            tmpsx = tmpsx->next;
        }
        printf("\n");
    } else if (strcmp(v,"libraries") == 0) {
        printf("LIBRARIES: \n");
        /* while (exp->list->next->val) */
        /*     printf(" %s\n", exp->list->next->val); */
        /* tmpsx = lookup(exp->list->next->val,d); */
        /* d = eval(tmpsx,d); */
        /* tmpsx = lookup(exp->list->next->next->val,d); */
        /* d = eval(tmpsx,d); */
    } else if (strcmp(v,"coq.pp") == 0) {
        printf("COQ.PP:\n");

        d = eval(exp->list->next,d);
        d = eval(exp->list->next->next,d);

    } else if (strcmp(v,"modules") == 0) {
        printf("MODULES:\n");

        d = eval(exp->list->next,d);
        d = eval(exp->list->next->next,d);

    } else if (strcmp(v,"point1") == 0) {
        printf("POINT1 OF :\n");
        tmpsx = lookup(exp->list->next->val,d);

        d = eval(tmpsx,d);

    } else if (strcmp(v,"point2") == 0) {
        printf("POINT2 OF :\n");

        tmpsx = lookup(exp->list->next->val,d);

        d = eval(tmpsx,d);

    } else {
        printf("EVAL: Unknown = %s\n",v);
    }

    return d;
}

/****
 * main
 ****/
int parse_dunefile(int argc, char **argv) {
    printf("parse_dunefile\n");
    char linebuf[BUFSIZ];
    FILE *fp;
    char *status;
    sexp_t *sx;
    dict_t *env = NULL;

    char *fname = "src/dune";

    fp = fopen(fname,"r+");
    if (fp == NULL) {
        errnum = errno;
        fprintf(stderr, "fopen fail for %s\n", fname);
        fprintf(stderr, "Value of errno: %d\n", errno);
        fprintf(stderr, "Error opening file %s: %s\n", fname, strerror( errnum ));
        exit(1);
    }
    printf("fopened %s\n", fname);

    while (1) {
        status = fgets(linebuf,BUFSIZ,fp);

        if (feof(fp) != 0) break;

        /* if not EOF and status was NULL, something bad happened. */
        if (status != linebuf) {
            printf("Error encountered on fgets.\n");
            exit(EXIT_FAILURE);
        }
        printf("readed: %s", linebuf);

        sx = parse_sexp(linebuf,BUFSIZ);

        if ( sx != NULL) {
            print_sexp(linebuf,BUFSIZ,sx);
            printf("parsed sexp: %s (%ld)\n", linebuf, strlen(linebuf));
            env = eval(sx,env);
        }

        destroy_sexp(sx);

        fflush(stderr);
    }

    purge_all(env);
    sexp_cleanup();

    fclose(fp);

    exit(EXIT_SUCCESS);
}
