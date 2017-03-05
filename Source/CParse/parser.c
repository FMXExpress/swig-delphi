/* A Bison parser, made by GNU Bison 2.4.2.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2006, 2009-2010 Free Software
   Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 25 "parser.y"

#define yylex yylex

#include "swig.h"
#include "cparse.h"
#include "preprocessor.h"
#include <ctype.h>

/* We do this for portability */
#undef alloca
#define alloca malloc

/* -----------------------------------------------------------------------------
 *                               Externals
 * ----------------------------------------------------------------------------- */

int  yyparse();

/* NEW Variables */

static Node    *top = 0;      /* Top of the generated parse tree */
static int      unnamed = 0;  /* Unnamed datatype counter */
static Hash    *classes = 0;        /* Hash table of classes */
static Hash    *classes_typedefs = 0; /* Hash table of typedef classes: typedef struct X {...} Y; */
static Symtab  *prev_symtab = 0;
static Node    *current_class = 0;
String  *ModuleName = 0;
static Node    *module_node = 0;
static String  *Classprefix = 0;  
static String  *Namespaceprefix = 0;
static int      inclass = 0;
static Node    *currentOuterClass = 0; /* for nested classes */
static const char *last_cpptype = 0;
static int      inherit_list = 0;
static Parm    *template_parameters = 0;
static int      extendmode   = 0;
static int      compact_default_args = 0;
static int      template_reduce = 0;
static int      cparse_externc = 0;
int		ignore_nested_classes = 0;
int		kwargs_supported = 0;
/* -----------------------------------------------------------------------------
 *                            Assist Functions
 * ----------------------------------------------------------------------------- */


 
/* Called by the parser (yyparse) when an error is found.*/
static void yyerror (const char *e) {
  (void)e;
}

/* Copies a node.  Does not copy tree links or symbol table data (except for
   sym:name) */
static String * checkT_Element_Replace(String * value) {
	if (strncmp(value, "T", 1) == 0) {
		return NewStringf("_%s", value);
	}
	return value;
}
static Node *copy_node(Node *n) {
  Node *nn;
  Iterator k;
  nn = NewHash();
  Setfile(nn,Getfile(n));
  Setline(nn,Getline(n));
  for (k = First(n); k.key; k = Next(k)) {
    String *ci;
    String *key = k.key;
    char *ckey = Char(key);
    if ((strcmp(ckey,"nextSibling") == 0) ||
	(strcmp(ckey,"previousSibling") == 0) ||
	(strcmp(ckey,"parentNode") == 0) ||
	(strcmp(ckey,"lastChild") == 0)) {
      continue;
    }
    if (Strncmp(key,"csym:",5) == 0) continue;
    /* We do copy sym:name.  For templates */
    if ((strcmp(ckey,"sym:name") == 0) || 
	(strcmp(ckey,"sym:weak") == 0) ||
	(strcmp(ckey,"sym:typename") == 0)) {
      String *ci = Copy(k.item);
      Setattr(nn,key, checkT_Element_Replace(ci));
      Delete(ci);
      continue;
    }
    if (strcmp(ckey,"sym:symtab") == 0) {
      Setattr(nn,"sym:needs_symtab", "1");
    }
    /* We don't copy any other symbol table attributes */
    if (strncmp(ckey,"sym:",4) == 0) {
      continue;
    }
    /* If children.  We copy them recursively using this function */
    if (strcmp(ckey,"firstChild") == 0) {
      /* Copy children */
      Node *cn = k.item;
      while (cn) {
	Node *copy = copy_node(cn);
	appendChild(nn,copy);
	Delete(copy);
	cn = nextSibling(cn);
      }
      continue;
    }
    /* We don't copy the symbol table.  But we drop an attribute 
       requires_symtab so that functions know it needs to be built */

    if (strcmp(ckey,"symtab") == 0) {
      /* Node defined a symbol table. */
      Setattr(nn,"requires_symtab","1");
      continue;
    }
    /* Can't copy nodes */
    if (strcmp(ckey,"node") == 0) {
      continue;
    }
    if ((strcmp(ckey,"parms") == 0) || (strcmp(ckey,"pattern") == 0) || (strcmp(ckey,"throws") == 0)
	|| (strcmp(ckey,"kwargs") == 0)) {
      ParmList *pl = CopyParmList(k.item);
      Setattr(nn,key,pl);
      Delete(pl);
      continue;
    }
    if (strcmp(ckey,"nested:outer") == 0) { /* don't copy outer classes links, they will be updated later */
      Setattr(nn, key, k.item);
      continue;
    }
    /* defaultargs will be patched back in later in update_defaultargs() */
    if (strcmp(ckey,"defaultargs") == 0) {
      Setattr(nn, "needs_defaultargs", "1");
      continue;
    }
    /* Looks okay.  Just copy the data using Copy */
    ci = Copy(k.item);
    Setattr(nn, key, checkT_Element_Replace(ci));
    Delete(ci);
  }
  return nn;
}

/* -----------------------------------------------------------------------------
 *                              Variables
 * ----------------------------------------------------------------------------- */

static char  *typemap_lang = 0;    /* Current language setting */

static int cplus_mode  = 0;

/* C++ modes */

#define  CPLUS_PUBLIC    1
#define  CPLUS_PRIVATE   2
#define  CPLUS_PROTECTED 3

/* include types */
static int   import_mode = 0;

void SWIG_typemap_lang(const char *tm_lang) {
  typemap_lang = Swig_copy_string(tm_lang);
}

void SWIG_cparse_set_compact_default_args(int defargs) {
  compact_default_args = defargs;
}

int SWIG_cparse_template_reduce(int treduce) {
  template_reduce = treduce;
  return treduce;  
}

/* -----------------------------------------------------------------------------
 *                           Assist functions
 * ----------------------------------------------------------------------------- */

static int promote_type(int t) {
  if (t <= T_UCHAR || t == T_CHAR) return T_INT;
  return t;
}

/* Perform type-promotion for binary operators */
static int promote(int t1, int t2) {
  t1 = promote_type(t1);
  t2 = promote_type(t2);
  return t1 > t2 ? t1 : t2;
}

static String *yyrename = 0;

/* Forward renaming operator */

static String *resolve_create_node_scope(String *cname);


Hash *Swig_cparse_features(void) {
  static Hash   *features_hash = 0;
  if (!features_hash) features_hash = NewHash();
  return features_hash;
}

/* Fully qualify any template parameters */
static String *feature_identifier_fix(String *s) {
  String *tp = SwigType_istemplate_templateprefix(s);
  if (tp) {
    String *ts, *ta, *tq;
    ts = SwigType_templatesuffix(s);
    ta = SwigType_templateargs(s);
    tq = Swig_symbol_type_qualify(ta,0);
    Append(tp,tq);
    Append(tp,ts);
    Delete(ts);
    Delete(ta);
    Delete(tq);
    return tp;
  } else {
    return NewString(s);
  }
}

static void set_access_mode(Node *n) {
  if (cplus_mode == CPLUS_PUBLIC)
    Setattr(n, "access", "public");
  else if (cplus_mode == CPLUS_PROTECTED)
    Setattr(n, "access", "protected");
  else
    Setattr(n, "access", "private");
}

static void restore_access_mode(Node *n) {
  String *mode = Getattr(n, "access");
  if (Strcmp(mode, "private") == 0)
    cplus_mode = CPLUS_PRIVATE;
  else if (Strcmp(mode, "protected") == 0)
    cplus_mode = CPLUS_PROTECTED;
  else
    cplus_mode = CPLUS_PUBLIC;
}

/* Generate the symbol table name for an object */
/* This is a bit of a mess. Need to clean up */
static String *add_oldname = 0;



static String *make_name(Node *n, String *name,SwigType *decl) {
  int destructor = name && (*(Char(name)) == '~');

  if (yyrename) {
    String *s = NewString(yyrename);
    Delete(yyrename);
    yyrename = 0;
    if (destructor  && (*(Char(s)) != '~')) {
      Insert(s,0,"~");
    }
    return s;
  }

  if (!name) return 0;
  return Swig_name_make(n,Namespaceprefix,name,decl,add_oldname);
}

/* Generate an unnamed identifier */
static String *make_unnamed() {
  unnamed++;
  return NewStringf("$unnamed%d$",unnamed);
}

/* Return if the node is a friend declaration */
static int is_friend(Node *n) {
  return Cmp(Getattr(n,"storage"),"friend") == 0;
}

static int is_operator(String *name) {
  return Strncmp(name,"operator ", 9) == 0;
}


/* Add declaration list to symbol table */
static int  add_only_one = 0;

static void add_symbols(Node *n) {
  String *decl;
  String *wrn = 0;

  if (inclass && n) {
    cparse_normalize_void(n);
  }
  while (n) {
    String *symname = 0;
    /* for friends, we need to pop the scope once */
    String *old_prefix = 0;
    Symtab *old_scope = 0;
    int isfriend = inclass && is_friend(n);
    int iscdecl = Cmp(nodeType(n),"cdecl") == 0;
    int only_csymbol = 0;
    
    if (inclass) {
      String *name = Getattr(n, "name");
      if (isfriend) {
	/* for friends, we need to add the scopename if needed */
	String *prefix = name ? Swig_scopename_prefix(name) : 0;
	old_prefix = Namespaceprefix;
	old_scope = Swig_symbol_popscope();
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	if (!prefix) {
	  if (name && !is_operator(name) && Namespaceprefix) {
	    String *nname = NewStringf("%s::%s", Namespaceprefix, name);
	    Setattr(n,"name",nname);
	    Delete(nname);
	  }
	} else {
	  Symtab *st = Swig_symbol_getscope(prefix);
	  String *ns = st ? Getattr(st,"name") : prefix;
	  String *base  = Swig_scopename_last(name);
	  String *nname = NewStringf("%s::%s", ns, base);
	  Setattr(n,"name",nname);
	  Delete(nname);
	  Delete(base);
	  Delete(prefix);
	}
	Namespaceprefix = 0;
      } else {
	/* for member functions, we need to remove the redundant
	   class scope if provided, as in
	   
	   struct Foo {
	   int Foo::method(int a);
	   };
	   
	*/
	String *prefix = name ? Swig_scopename_prefix(name) : 0;
	if (prefix) {
	  if (Classprefix && (Equal(prefix,Classprefix))) {
	    String *base = Swig_scopename_last(name);
	    Setattr(n,"name",base);
	    Delete(base);
	  }
	  Delete(prefix);
	}
      }
    }

    if (!isfriend && (inclass || extendmode)) {
      Setattr(n,"ismember","1");
    }

    if (extendmode) {
      Setattr(n,"isextendmember","1");
    }

    if (!isfriend && inclass) {
      if ((cplus_mode != CPLUS_PUBLIC)) {
	only_csymbol = 1;
	if (cplus_mode == CPLUS_PROTECTED) {
	  Setattr(n,"access", "protected");
	  only_csymbol = !Swig_need_protected(n);
	} else {
	  Setattr(n,"access", "private");
	  /* private are needed only when they are pure virtuals - why? */
	  if ((Cmp(Getattr(n,"storage"),"virtual") == 0) && (Cmp(Getattr(n,"value"),"0") == 0)) {
	    only_csymbol = 0;
	  }
	  if (Cmp(nodeType(n),"destructor") == 0) {
	    /* Needed for "unref" feature */
	    only_csymbol = 0;
	  }
	}
      } else {
	  Setattr(n,"access", "public");
      }
    }
    if (Getattr(n,"sym:name")) {
      n = nextSibling(n);
      continue;
    }
    decl = Getattr(n,"decl");
    if (!SwigType_isfunction(decl)) {
      String *name = Getattr(n,"name");
      String *makename = Getattr(n,"parser:makename");
      if (iscdecl) {	
	String *storage = Getattr(n, "storage");
	if (Cmp(storage,"typedef") == 0) {
	  Setattr(n,"kind","typedef");
	} else {
	  SwigType *type = Getattr(n,"type");
	  String *value = Getattr(n,"value");
	  Setattr(n,"kind","variable");
	  if (value && Len(value)) {
	    Setattr(n,"hasvalue","1");
	  }
	  if (type) {
	    SwigType *ty;
	    SwigType *tmp = 0;
	    if (decl) {
	      ty = tmp = Copy(type);
	      SwigType_push(ty,decl);
	    } else {
	      ty = type;
	    }
	    if (!SwigType_ismutable(ty) || (storage && Strstr(storage, "constexpr"))) {
	      SetFlag(n,"hasconsttype");
	      SetFlag(n,"feature:immutable");
	    }
	    if (tmp) Delete(tmp);
	  }
	  if (!type) {
	    Printf(stderr,"notype name %s\n", name);
	  }
	}
      }
      Swig_features_get(Swig_cparse_features(), Namespaceprefix, name, 0, n);
      if (makename) {
	symname = make_name(n, makename,0);
        Delattr(n,"parser:makename"); /* temporary information, don't leave it hanging around */
      } else {
        makename = name;
	symname = make_name(n, makename,0);
      }
      
      if (!symname) {
	symname = Copy(Getattr(n,"unnamed"));
      }
      if (symname) {
	wrn = Swig_name_warning(n, Namespaceprefix, symname,0);
      }
    } else {
      String *name = Getattr(n,"name");
      SwigType *fdecl = Copy(decl);
      SwigType *fun = SwigType_pop_function(fdecl);
      if (iscdecl) {	
	Setattr(n,"kind","function");
      }
      
      Swig_features_get(Swig_cparse_features(),Namespaceprefix,name,fun,n);

      symname = make_name(n, name,fun);
      wrn = Swig_name_warning(n, Namespaceprefix,symname,fun);
      
      Delete(fdecl);
      Delete(fun);
      
    }
    if (!symname) {
      n = nextSibling(n);
      continue;
    }
    if (cparse_cplusplus) {
      String *value = Getattr(n, "value");
      if (value && Strcmp(value, "delete") == 0) {
	/* C++11 deleted definition / deleted function */
        SetFlag(n,"deleted");
        SetFlag(n,"feature:ignore");
      }
    }
    if (only_csymbol || GetFlag(n,"feature:ignore") || strncmp(Char(symname),"$ignore",7) == 0) {
      /* Only add to C symbol table and continue */
      Swig_symbol_add(0, n);
      if (!only_csymbol && !GetFlag(n, "feature:ignore")) {
	/* Print the warning attached to $ignore name, if any */
        char *c = Char(symname) + 7;
	if (strlen(c)) {
	  SWIG_WARN_NODE_BEGIN(n);
	  Swig_warning(0,Getfile(n), Getline(n), "%s\n",c+1);
	  SWIG_WARN_NODE_END(n);
	}
	/* If the symbol was ignored via "rename" and is visible, set also feature:ignore*/
	SetFlag(n, "feature:ignore");
      }
      if (!GetFlag(n, "feature:ignore") && Strcmp(symname,"$ignore") == 0) {
	/* Add feature:ignore if the symbol was explicitely ignored, regardless of visibility */
	SetFlag(n, "feature:ignore");
      }
    } else {
      Node *c;
      if ((wrn) && (Len(wrn))) {
	String *metaname = symname;
	if (!Getmeta(metaname,"already_warned")) {
	  SWIG_WARN_NODE_BEGIN(n);
	  Swig_warning(0,Getfile(n),Getline(n), "%s\n", wrn);
	  SWIG_WARN_NODE_END(n);
	  Setmeta(metaname,"already_warned","1");
	}
      }
      c = Swig_symbol_add(symname,n);

      if (c != n) {
        /* symbol conflict attempting to add in the new symbol */
        if (Getattr(n,"sym:weak")) {
          Setattr(n,"sym:name",symname);
        } else {
          String *e = NewStringEmpty();
          String *en = NewStringEmpty();
          String *ec = NewStringEmpty();
          int redefined = Swig_need_redefined_warn(n,c,inclass);
          if (redefined) {
            Printf(en,"Identifier '%s' redefined (ignored)",symname);
            Printf(ec,"previous definition of '%s'",symname);
          } else {
            Printf(en,"Redundant redeclaration of '%s'",symname);
            Printf(ec,"previous declaration of '%s'",symname);
          }
          if (Cmp(symname,Getattr(n,"name"))) {
            Printf(en," (Renamed from '%s')", SwigType_namestr(Getattr(n,"name")));
          }
          Printf(en,",");
          if (Cmp(symname,Getattr(c,"name"))) {
            Printf(ec," (Renamed from '%s')", SwigType_namestr(Getattr(c,"name")));
          }
          Printf(ec,".");
	  SWIG_WARN_NODE_BEGIN(n);
          if (redefined) {
            Swig_warning(WARN_PARSE_REDEFINED,Getfile(n),Getline(n),"%s\n",en);
            Swig_warning(WARN_PARSE_REDEFINED,Getfile(c),Getline(c),"%s\n",ec);
          } else if (!is_friend(n) && !is_friend(c)) {
            Swig_warning(WARN_PARSE_REDUNDANT,Getfile(n),Getline(n),"%s\n",en);
            Swig_warning(WARN_PARSE_REDUNDANT,Getfile(c),Getline(c),"%s\n",ec);
          }
	  SWIG_WARN_NODE_END(n);
          Printf(e,"%s:%d:%s\n%s:%d:%s\n",Getfile(n),Getline(n),en,
                 Getfile(c),Getline(c),ec);
          Setattr(n,"error",e);
	  Delete(e);
          Delete(en);
          Delete(ec);
        }
      }
    }
    /* restore the class scope if needed */
    if (isfriend) {
      Swig_symbol_setscope(old_scope);
      if (old_prefix) {
	Delete(Namespaceprefix);
	Namespaceprefix = old_prefix;
      }
    }
    Delete(symname);

    if (add_only_one) return;
    n = nextSibling(n);
  }
}


/* add symbols a parse tree node copy */

static void add_symbols_copy(Node *n) {
  String *name;
  int    emode = 0;
  while (n) {
    char *cnodeType = Char(nodeType(n));

    if (strcmp(cnodeType,"access") == 0) {
      String *kind = Getattr(n,"kind");
      if (Strcmp(kind,"public") == 0) {
	cplus_mode = CPLUS_PUBLIC;
      } else if (Strcmp(kind,"private") == 0) {
	cplus_mode = CPLUS_PRIVATE;
      } else if (Strcmp(kind,"protected") == 0) {
	cplus_mode = CPLUS_PROTECTED;
      }
      n = nextSibling(n);
      continue;
    }

    add_oldname = Getattr(n,"sym:name");
    if ((add_oldname) || (Getattr(n,"sym:needs_symtab"))) {
      int old_inclass = -1;
      Node *old_current_class = 0;
      if (add_oldname) {
	DohIncref(add_oldname);
	/*  Disable this, it prevents %rename to work with templates */
	/* If already renamed, we used that name  */
	/*
	if (Strcmp(add_oldname, Getattr(n,"name")) != 0) {
	  Delete(yyrename);
	  yyrename = Copy(add_oldname);
	}
	*/
      }
      Delattr(n,"sym:needs_symtab");
      Delattr(n,"sym:name");

      add_only_one = 1;
      add_symbols(n);

      if (Getattr(n,"partialargs")) {
	Swig_symbol_cadd(Getattr(n,"partialargs"),n);
      }
      add_only_one = 0;
      name = Getattr(n,"name");
      if (Getattr(n,"requires_symtab")) {
	Swig_symbol_newscope();
	Swig_symbol_setscopename(name);
	Delete(Namespaceprefix);
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
      }
      if (strcmp(cnodeType,"class") == 0) {
	old_inclass = inclass;
	inclass = 1;
	old_current_class = current_class;
	current_class = n;
	if (Strcmp(Getattr(n,"kind"),"class") == 0) {
	  cplus_mode = CPLUS_PRIVATE;
	} else {
	  cplus_mode = CPLUS_PUBLIC;
	}
      }
      if (strcmp(cnodeType,"extend") == 0) {
	emode = cplus_mode;
	cplus_mode = CPLUS_PUBLIC;
      }
      add_symbols_copy(firstChild(n));
      if (strcmp(cnodeType,"extend") == 0) {
	cplus_mode = emode;
      }
      if (Getattr(n,"requires_symtab")) {
	Setattr(n,"symtab", Swig_symbol_popscope());
	Delattr(n,"requires_symtab");
	Delete(Namespaceprefix);
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
      }
      if (add_oldname) {
	Delete(add_oldname);
	add_oldname = 0;
      }
      if (strcmp(cnodeType,"class") == 0) {
	inclass = old_inclass;
	current_class = old_current_class;
      }
    } else {
      if (strcmp(cnodeType,"extend") == 0) {
	emode = cplus_mode;
	cplus_mode = CPLUS_PUBLIC;
      }
      add_symbols_copy(firstChild(n));
      if (strcmp(cnodeType,"extend") == 0) {
	cplus_mode = emode;
      }
    }
    n = nextSibling(n);
  }
}

/* Add in the "defaultargs" attribute for functions in instantiated templates.
 * n should be any instantiated template (class or start of linked list of functions). */
static void update_defaultargs(Node *n) {
  if (n) {
    Node *firstdefaultargs = n;
    update_defaultargs(firstChild(n));
    n = nextSibling(n);
    /* recursively loop through nodes of all types, but all we really need are the overloaded functions */
    while (n) {
      update_defaultargs(firstChild(n));
      if (!Getattr(n, "defaultargs")) {
	if (Getattr(n, "needs_defaultargs")) {
	  Setattr(n, "defaultargs", firstdefaultargs);
	  Delattr(n, "needs_defaultargs");
	} else {
	  firstdefaultargs = n;
	}
      } else {
	/* Functions added in with %extend (for specialized template classes) will already have default args patched up */
	assert(Getattr(n, "defaultargs") == firstdefaultargs);
      }
      n = nextSibling(n);
    }
  }
}

/* Check a set of declarations to see if any are pure-abstract */

static List *pure_abstracts(Node *n) {
  List *abstracts = 0;
  while (n) {
    if (Cmp(nodeType(n),"cdecl") == 0) {
      String *decl = Getattr(n,"decl");
      if (SwigType_isfunction(decl)) {
	String *init = Getattr(n,"value");
	if (Cmp(init,"0") == 0) {
	  if (!abstracts) {
	    abstracts = NewList();
	  }
	  Append(abstracts,n);
	  SetFlag(n,"abstract");
	}
      }
    } else if (Cmp(nodeType(n),"destructor") == 0) {
      if (Cmp(Getattr(n,"value"),"0") == 0) {
	if (!abstracts) {
	  abstracts = NewList();
	}
	Append(abstracts,n);
	SetFlag(n,"abstract");
      }
    }
    n = nextSibling(n);
  }
  return abstracts;
}

/* Make a classname */

static String *make_class_name(String *name) {
  String *nname = 0;
  String *prefix;
  if (Namespaceprefix) {
    nname= NewStringf("%s::%s", Namespaceprefix, name);
  } else {
    nname = NewString(name);
  }
  prefix = SwigType_istemplate_templateprefix(nname);
  if (prefix) {
    String *args, *qargs;
    args   = SwigType_templateargs(nname);
    qargs  = Swig_symbol_type_qualify(args,0);
    Append(prefix,qargs);
    Delete(nname);
    Delete(args);
    Delete(qargs);
    nname = prefix;
  }
  return nname;
}

/* Use typedef name as class name */

static void add_typedef_name(Node *n, Node *declnode, String *oldName, Symtab *cscope, String *scpname) {
  String *class_rename = 0;
  SwigType *decl = Getattr(declnode, "decl");
  if (!decl || !Len(decl)) {
    String *cname;
    String *tdscopename;
    String *class_scope = Swig_symbol_qualifiedscopename(cscope);
    String *name = Getattr(declnode, "name");
    cname = Copy(name);
    Setattr(n, "tdname", cname);
    tdscopename = class_scope ? NewStringf("%s::%s", class_scope, name) : Copy(name);
    class_rename = Getattr(n, "class_rename");
    if (class_rename && (Strcmp(class_rename, oldName) == 0))
      Setattr(n, "class_rename", NewString(name));
    if (!classes_typedefs) classes_typedefs = NewHash();
    if (!Equal(scpname, tdscopename) && !Getattr(classes_typedefs, tdscopename)) {
      Setattr(classes_typedefs, tdscopename, n);
    }
    Setattr(n, "decl", decl);
    Delete(class_scope);
    Delete(cname);
    Delete(tdscopename);
  }
}

/* If the class name is qualified.  We need to create or lookup namespace entries */

static Symtab *set_scope_to_global() {
  Symtab *symtab = Swig_symbol_global_scope();
  Swig_symbol_setscope(symtab);
  return symtab;
}
 
/* Remove the block braces, { and }, if the 'noblock' attribute is set.
 * Node *kw can be either a Hash or Parmlist. */
static String *remove_block(Node *kw, const String *inputcode) {
  String *modified_code = 0;
  while (kw) {
   String *name = Getattr(kw,"name");
   if (name && (Cmp(name,"noblock") == 0)) {
     char *cstr = Char(inputcode);
     int len = Len(inputcode);
     if (len && cstr[0] == '{') {
       --len; ++cstr; 
       if (len && cstr[len - 1] == '}') { --len; }
       /* we now remove the extra spaces */
       while (len && isspace((int)cstr[0])) { --len; ++cstr; }
       while (len && isspace((int)cstr[len - 1])) { --len; }
       modified_code = NewStringWithSize(cstr, len);
       break;
     }
   }
   kw = nextSibling(kw);
  }
  return modified_code;
}


static Node *nscope = 0;
static Node *nscope_inner = 0;

/* Remove the scope prefix from cname and return the base name without the prefix.
 * The scopes required for the symbol name are resolved and/or created, if required.
 * For example AA::BB::CC as input returns CC and creates the namespace AA then inner 
 * namespace BB in the current scope. If cname is found to already exist as a weak symbol
 * (forward reference) then the scope might be changed to match, such as when a symbol match 
 * is made via a using reference. */
static String *resolve_create_node_scope(String *cname) {
  Symtab *gscope = 0;
  Node *cname_node = 0;
  int skip_lookup = 0;
  nscope = 0;
  nscope_inner = 0;  

  if (Strncmp(cname,"::",2) == 0)
    skip_lookup = 1;

  cname_node = skip_lookup ? 0 : Swig_symbol_clookup_no_inherit(cname, 0);

  if (cname_node) {
    /* The symbol has been defined already or is in another scope.
       If it is a weak symbol, it needs replacing and if it was brought into the current scope
       via a using declaration, the scope needs adjusting appropriately for the new symbol.
       Similarly for defined templates. */
    Symtab *symtab = Getattr(cname_node, "sym:symtab");
    Node *sym_weak = Getattr(cname_node, "sym:weak");
    if ((symtab && sym_weak) || Equal(nodeType(cname_node), "template")) {
      /* Check if the scope is the current scope */
      String *current_scopename = Swig_symbol_qualifiedscopename(0);
      String *found_scopename = Swig_symbol_qualifiedscopename(symtab);
      int len;
      if (!current_scopename)
	current_scopename = NewString("");
      if (!found_scopename)
	found_scopename = NewString("");
      len = Len(current_scopename);
      if ((len > 0) && (Strncmp(current_scopename, found_scopename, len) == 0)) {
	if (Len(found_scopename) > len + 2) {
	  /* A matching weak symbol was found in non-global scope, some scope adjustment may be required */
	  String *new_cname = NewString(Char(found_scopename) + len + 2); /* skip over "::" prefix */
	  String *base = Swig_scopename_last(cname);
	  Printf(new_cname, "::%s", base);
	  cname = new_cname;
	  Delete(base);
	} else {
	  /* A matching weak symbol was found in the same non-global local scope, no scope adjustment required */
	  assert(len == Len(found_scopename));
	}
      } else {
	String *base = Swig_scopename_last(cname);
	if (Len(found_scopename) > 0) {
	  /* A matching weak symbol was found in a different scope to the local scope - probably via a using declaration */
	  cname = NewStringf("%s::%s", found_scopename, base);
	} else {
	  /* Either:
	      1) A matching weak symbol was found in a different scope to the local scope - this is actually a
	      symbol with the same name in a different scope which we don't want, so no adjustment required.
	      2) A matching weak symbol was found in the global scope - no adjustment required.
	  */
	  cname = Copy(base);
	}
	Delete(base);
      }
      Delete(current_scopename);
      Delete(found_scopename);
    }
  }

  if (Swig_scopename_check(cname)) {
    Node   *ns;
    String *prefix = Swig_scopename_prefix(cname);
    String *base = Swig_scopename_last(cname);
    if (prefix && (Strncmp(prefix,"::",2) == 0)) {
/* I don't think we can use :: global scope to declare classes and hence neither %template. - consider reporting error instead - wsfulton. */
      /* Use the global scope */
      String *nprefix = NewString(Char(prefix)+2);
      Delete(prefix);
      prefix= nprefix;
      gscope = set_scope_to_global();
    }
    if (Len(prefix) == 0) {
      /* Use the global scope, but we need to add a 'global' namespace.  */
      if (!gscope) gscope = set_scope_to_global();
      /* note that this namespace is not the "unnamed" one,
	 and we don't use Setattr(nscope,"name", ""),
	 because the unnamed namespace is private */
      nscope = new_node("namespace");
      Setattr(nscope,"symtab", gscope);;
      nscope_inner = nscope;
      return base;
    }
    /* Try to locate the scope */
    ns = Swig_symbol_clookup(prefix,0);
    if (!ns) {
      Swig_error(cparse_file,cparse_line,"Undefined scope '%s'\n", prefix);
    } else {
      Symtab *nstab = Getattr(ns,"symtab");
      if (!nstab) {
	Swig_error(cparse_file,cparse_line, "'%s' is not defined as a valid scope.\n", prefix);
	ns = 0;
      } else {
	/* Check if the node scope is the current scope */
	String *tname = Swig_symbol_qualifiedscopename(0);
	String *nname = Swig_symbol_qualifiedscopename(nstab);
	if (tname && (Strcmp(tname,nname) == 0)) {
	  ns = 0;
	  cname = base;
	}
	Delete(tname);
	Delete(nname);
      }
      if (ns) {
	/* we will try to create a new node using the namespaces we
	   can find in the scope name */
	List *scopes;
	String *sname;
	Iterator si;
	String *name = NewString(prefix);
	scopes = NewList();
	while (name) {
	  String *base = Swig_scopename_last(name);
	  String *tprefix = Swig_scopename_prefix(name);
	  Insert(scopes,0,base);
	  Delete(base);
	  Delete(name);
	  name = tprefix;
	}
	for (si = First(scopes); si.item; si = Next(si)) {
	  Node *ns1,*ns2;
	  sname = si.item;
	  ns1 = Swig_symbol_clookup(sname,0);
	  assert(ns1);
	  if (Strcmp(nodeType(ns1),"namespace") == 0) {
	    if (Getattr(ns1,"alias")) {
	      ns1 = Getattr(ns1,"namespace");
	    }
	  } else {
	    /* now this last part is a class */
	    si = Next(si);
	    /*  or a nested class tree, which is unrolled here */
	    for (; si.item; si = Next(si)) {
	      if (si.item) {
		Printf(sname,"::%s",si.item);
	      }
	    }
	    /* we get the 'inner' class */
	    nscope_inner = Swig_symbol_clookup(sname,0);
	    /* set the scope to the inner class */
	    Swig_symbol_setscope(Getattr(nscope_inner,"symtab"));
	    /* save the last namespace prefix */
	    Delete(Namespaceprefix);
	    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	    /* and return the node name, including the inner class prefix */
	    break;
	  }
	  /* here we just populate the namespace tree as usual */
	  ns2 = new_node("namespace");
	  Setattr(ns2,"name",sname);
	  Setattr(ns2,"symtab", Getattr(ns1,"symtab"));
	  add_symbols(ns2);
	  Swig_symbol_setscope(Getattr(ns1,"symtab"));
	  Delete(Namespaceprefix);
	  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	  if (nscope_inner) {
	    if (Getattr(nscope_inner,"symtab") != Getattr(ns2,"symtab")) {
	      appendChild(nscope_inner,ns2);
	      Delete(ns2);
	    }
	  }
	  nscope_inner = ns2;
	  if (!nscope) nscope = ns2;
	}
	cname = base;
	Delete(scopes);
      }
    }
    Delete(prefix);
  }

  return cname;
}
 
/* look for simple typedef name in typedef list */
static String *try_to_find_a_name_for_unnamed_structure(const char *storage, Node *decls) {
  String *name = 0;
  Node *n = decls;
  if (storage && (strcmp(storage, "typedef") == 0)) {
    for (; n; n = nextSibling(n)) {
      if (!Len(Getattr(n, "decl"))) {
	name = Copy(Getattr(n, "name"));
	break;
      }
    }
  }
  return name;
}

/* traverse copied tree segment, and update outer class links*/
static void update_nested_classes(Node *n)
{
  Node *c = firstChild(n);
  while (c) {
    if (Getattr(c, "nested:outer"))
      Setattr(c, "nested:outer", n);
    update_nested_classes(c);
    c = nextSibling(c);
  }
}

/* -----------------------------------------------------------------------------
 * nested_forward_declaration()
 * 
 * Nested struct handling for C++ code if the nested classes are disabled.
 * Create the nested class/struct/union as a forward declaration.
 * ----------------------------------------------------------------------------- */

static Node *nested_forward_declaration(const char *storage, const char *kind, String *sname, String *name, Node *cpp_opt_declarators) {
  Node *nn = 0;

  if (sname) {
    /* Add forward declaration of the nested type */
    Node *n = new_node("classforward");
    Setattr(n, "kind", kind);
    Setattr(n, "name", sname);
    Setattr(n, "storage", storage);
    Setattr(n, "sym:weak", "1");
    add_symbols(n);
    nn = n;
  }

  /* Add any variable instances. Also add in any further typedefs of the nested type.
     Note that anonymous typedefs (eg typedef struct {...} a, b;) are treated as class forward declarations */
  if (cpp_opt_declarators) {
    int storage_typedef = (storage && (strcmp(storage, "typedef") == 0));
    int variable_of_anonymous_type = !sname && !storage_typedef;
    if (!variable_of_anonymous_type) {
      int anonymous_typedef = !sname && (storage && (strcmp(storage, "typedef") == 0));
      Node *n = cpp_opt_declarators;
      SwigType *type = name;
      while (n) {
	Setattr(n, "type", type);
	Setattr(n, "storage", storage);
	if (anonymous_typedef) {
	  Setattr(n, "nodeType", "classforward");
	  Setattr(n, "sym:weak", "1");
	}
	n = nextSibling(n);
      }
      add_symbols(cpp_opt_declarators);

      if (nn) {
	set_nextSibling(nn, cpp_opt_declarators);
      } else {
	nn = cpp_opt_declarators;
      }
    }
  }

  if (!currentOuterClass || !GetFlag(currentOuterClass, "nested")) {
    if (nn && Equal(nodeType(nn), "classforward")) {
      Node *n = nn;
      if (!GetFlag(n, "feature:ignore")) {
	SWIG_WARN_NODE_BEGIN(n);
	Swig_warning(WARN_PARSE_NAMED_NESTED_CLASS, cparse_file, cparse_line,"Nested %s not currently supported (%s ignored)\n", kind, sname ? sname : name);
	SWIG_WARN_NODE_END(n);
      }
    } else {
      Swig_warning(WARN_PARSE_UNNAMED_NESTED_CLASS, cparse_file, cparse_line, "Nested %s not currently supported (ignored).\n", kind);
    }
  }

  return nn;
}


Node *Swig_cparse(File *f) {
  scanner_file(f);
  top = 0;
  yyparse();
  return top;
}

static void single_new_feature(const char *featurename, String *val, Hash *featureattribs, char *declaratorid, SwigType *type, ParmList *declaratorparms, String *qualifier) {
  String *fname;
  String *name;
  String *fixname;
  SwigType *t = Copy(type);

  /* Printf(stdout, "single_new_feature: [%s] [%s] [%s] [%s] [%s] [%s]\n", featurename, val, declaratorid, t, ParmList_str_defaultargs(declaratorparms), qualifier); */

  /* Warn about deprecated features */
  if (strcmp(featurename, "nestedworkaround") == 0)
    Swig_warning(WARN_DEPRECATED_NESTED_WORKAROUND, cparse_file, cparse_line, "The 'nestedworkaround' feature is deprecated.\n");

  fname = NewStringf("feature:%s",featurename);
  if (declaratorid) {
    fixname = feature_identifier_fix(declaratorid);
  } else {
    fixname = NewStringEmpty();
  }
  if (Namespaceprefix) {
    name = NewStringf("%s::%s",Namespaceprefix, fixname);
  } else {
    name = fixname;
  }

  if (declaratorparms) Setmeta(val,"parms",declaratorparms);
  if (!Len(t)) t = 0;
  if (t) {
    if (qualifier) SwigType_push(t,qualifier);
    if (SwigType_isfunction(t)) {
      SwigType *decl = SwigType_pop_function(t);
      if (SwigType_ispointer(t)) {
	String *nname = NewStringf("*%s",name);
	Swig_feature_set(Swig_cparse_features(), nname, decl, fname, val, featureattribs);
	Delete(nname);
      } else {
	Swig_feature_set(Swig_cparse_features(), name, decl, fname, val, featureattribs);
      }
      Delete(decl);
    } else if (SwigType_ispointer(t)) {
      String *nname = NewStringf("*%s",name);
      Swig_feature_set(Swig_cparse_features(),nname,0,fname,val, featureattribs);
      Delete(nname);
    }
  } else {
    /* Global feature, that is, feature not associated with any particular symbol */
    Swig_feature_set(Swig_cparse_features(),name,0,fname,val, featureattribs);
  }
  Delete(fname);
  Delete(name);
}

/* Add a new feature to the Hash. Additional features are added if the feature has a parameter list (declaratorparms)
 * and one or more of the parameters have a default argument. An extra feature is added for each defaulted parameter,
 * simulating the equivalent overloaded method. */
static void new_feature(const char *featurename, String *val, Hash *featureattribs, char *declaratorid, SwigType *type, ParmList *declaratorparms, String *qualifier) {

  ParmList *declparms = declaratorparms;

  /* remove the { and } braces if the noblock attribute is set */
  String *newval = remove_block(featureattribs, val);
  val = newval ? newval : val;

  /* Add the feature */
  single_new_feature(featurename, val, featureattribs, declaratorid, type, declaratorparms, qualifier);

  /* Add extra features if there are default parameters in the parameter list */
  if (type) {
    while (declparms) {
      if (ParmList_has_defaultargs(declparms)) {

        /* Create a parameter list for the new feature by copying all
           but the last (defaulted) parameter */
        ParmList* newparms = CopyParmListMax(declparms, ParmList_len(declparms)-1);

        /* Create new declaration - with the last parameter removed */
        SwigType *newtype = Copy(type);
        Delete(SwigType_pop_function(newtype)); /* remove the old parameter list from newtype */
        SwigType_add_function(newtype,newparms);

        single_new_feature(featurename, Copy(val), featureattribs, declaratorid, newtype, newparms, qualifier);
        declparms = newparms;
      } else {
        declparms = 0;
      }
    }
  }
}

/* check if a function declaration is a plain C object */
static int is_cfunction(Node *n) {
  if (!cparse_cplusplus || cparse_externc)
    return 1;
  if (Swig_storage_isexternc(n)) {
    return 1;
  }
  return 0;
}

/* If the Node is a function with parameters, check to see if any of the parameters
 * have default arguments. If so create a new function for each defaulted argument. 
 * The additional functions form a linked list of nodes with the head being the original Node n. */
static void default_arguments(Node *n) {
  Node *function = n;

  if (function) {
    ParmList *varargs = Getattr(function,"feature:varargs");
    if (varargs) {
      /* Handles the %varargs directive by looking for "feature:varargs" and 
       * substituting ... with an alternative set of arguments.  */
      Parm     *p = Getattr(function,"parms");
      Parm     *pp = 0;
      while (p) {
	SwigType *t = Getattr(p,"type");
	if (Strcmp(t,"v(...)") == 0) {
	  if (pp) {
	    ParmList *cv = Copy(varargs);
	    set_nextSibling(pp,cv);
	    Delete(cv);
	  } else {
	    ParmList *cv =  Copy(varargs);
	    Setattr(function,"parms", cv);
	    Delete(cv);
	  }
	  break;
	}
	pp = p;
	p = nextSibling(p);
      }
    }

    /* Do not add in functions if kwargs is being used or if user wants old default argument wrapping
       (one wrapped method per function irrespective of number of default arguments) */
    if (compact_default_args 
	|| is_cfunction(function) 
	|| GetFlag(function,"feature:compactdefaultargs") 
	|| (GetFlag(function,"feature:kwargs") && kwargs_supported)) {
      ParmList *p = Getattr(function,"parms");
      if (p) 
        Setattr(p,"compactdefargs", "1"); /* mark parameters for special handling */
      function = 0; /* don't add in extra methods */
    }
  }

  while (function) {
    ParmList *parms = Getattr(function,"parms");
    if (ParmList_has_defaultargs(parms)) {

      /* Create a parameter list for the new function by copying all
         but the last (defaulted) parameter */
      ParmList* newparms = CopyParmListMax(parms,ParmList_len(parms)-1);

      /* Create new function and add to symbol table */
      {
	SwigType *ntype = Copy(nodeType(function));
	char *cntype = Char(ntype);
        Node *new_function = new_node(ntype);
        SwigType *decl = Copy(Getattr(function,"decl"));
        int constqualifier = SwigType_isconst(decl);
	String *ccode = Copy(Getattr(function,"code"));
	String *cstorage = Copy(Getattr(function,"storage"));
	String *cvalue = Copy(Getattr(function,"value"));
	SwigType *ctype = Copy(Getattr(function,"type"));
	String *cthrow = Copy(Getattr(function,"throw"));

        Delete(SwigType_pop_function(decl)); /* remove the old parameter list from decl */
        SwigType_add_function(decl,newparms);
        if (constqualifier)
          SwigType_add_qualifier(decl,"const");

        Setattr(new_function,"name", Getattr(function,"name"));
        Setattr(new_function,"code", ccode);
        Setattr(new_function,"decl", decl);
        Setattr(new_function,"parms", newparms);
        Setattr(new_function,"storage", cstorage);
        Setattr(new_function,"value", cvalue);
        Setattr(new_function,"type", ctype);
        Setattr(new_function,"throw", cthrow);

	Delete(ccode);
	Delete(cstorage);
	Delete(cvalue);
	Delete(ctype);
	Delete(cthrow);
	Delete(decl);

        {
          Node *throws = Getattr(function,"throws");
	  ParmList *pl = CopyParmList(throws);
          if (throws) Setattr(new_function,"throws",pl);
	  Delete(pl);
        }

        /* copy specific attributes for global (or in a namespace) template functions - these are not templated class methods */
        if (strcmp(cntype,"template") == 0) {
          Node *templatetype = Getattr(function,"templatetype");
          Node *symtypename = Getattr(function,"sym:typename");
          Parm *templateparms = Getattr(function,"templateparms");
          if (templatetype) {
	    Node *tmp = Copy(templatetype);
	    Setattr(new_function,"templatetype",tmp);
	    Delete(tmp);
	  }
          if (symtypename) {
	    Node *tmp = Copy(symtypename);
	    Setattr(new_function,"sym:typename",tmp);
	    Delete(tmp);
	  }
          if (templateparms) {
	    Parm *tmp = CopyParmList(templateparms);
	    Setattr(new_function,"templateparms",tmp);
	    Delete(tmp);
	  }
        } else if (strcmp(cntype,"constructor") == 0) {
          /* only copied for constructors as this is not a user defined feature - it is hard coded in the parser */
          if (GetFlag(function,"feature:new")) SetFlag(new_function,"feature:new");
        }

        add_symbols(new_function);
        /* mark added functions as ones with overloaded parameters and point to the parsed method */
        Setattr(new_function,"defaultargs", n);

        /* Point to the new function, extending the linked list */
        set_nextSibling(function, new_function);
	Delete(new_function);
        function = new_function;
	
	Delete(ntype);
      }
    } else {
      function = 0;
    }
  }
}

/* -----------------------------------------------------------------------------
 * mark_nodes_as_extend()
 *
 * Used by the %extend to mark subtypes with "feature:extend".
 * template instances declared within %extend are skipped
 * ----------------------------------------------------------------------------- */

static void mark_nodes_as_extend(Node *n) {
  for (; n; n = nextSibling(n)) {
    if (Getattr(n, "template") && Strcmp(nodeType(n), "class") == 0)
      continue;
    /* Fix me: extend is not a feature. Replace with isextendmember? */
    Setattr(n, "feature:extend", "1");
    mark_nodes_as_extend(firstChild(n));
  }
}



/* Line 189 of yacc.c  */
#line 1394 "CParse/parser.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ID = 258,
     HBLOCK = 259,
     POUND = 260,
     STRING = 261,
     WSTRING = 262,
     INCLUDE = 263,
     IMPORT = 264,
     INSERT = 265,
     CHARCONST = 266,
     WCHARCONST = 267,
     NUM_INT = 268,
     NUM_FLOAT = 269,
     NUM_UNSIGNED = 270,
     NUM_LONG = 271,
     NUM_ULONG = 272,
     NUM_LONGLONG = 273,
     NUM_ULONGLONG = 274,
     NUM_BOOL = 275,
     TYPEDEF = 276,
     TYPE_INT = 277,
     TYPE_UNSIGNED = 278,
     TYPE_SHORT = 279,
     TYPE_LONG = 280,
     TYPE_FLOAT = 281,
     TYPE_DOUBLE = 282,
     TYPE_CHAR = 283,
     TYPE_WCHAR = 284,
     TYPE_VOID = 285,
     TYPE_SIGNED = 286,
     TYPE_BOOL = 287,
     TYPE_COMPLEX = 288,
     TYPE_TYPEDEF = 289,
     TYPE_RAW = 290,
     TYPE_NON_ISO_INT8 = 291,
     TYPE_NON_ISO_INT16 = 292,
     TYPE_NON_ISO_INT32 = 293,
     TYPE_NON_ISO_INT64 = 294,
     LPAREN = 295,
     RPAREN = 296,
     COMMA = 297,
     SEMI = 298,
     EXTERN = 299,
     INIT = 300,
     LBRACE = 301,
     RBRACE = 302,
     PERIOD = 303,
     CONST_QUAL = 304,
     VOLATILE = 305,
     REGISTER = 306,
     STRUCT = 307,
     UNION = 308,
     EQUAL = 309,
     SIZEOF = 310,
     MODULE = 311,
     LBRACKET = 312,
     RBRACKET = 313,
     BEGINFILE = 314,
     ENDOFFILE = 315,
     ILLEGAL = 316,
     CONSTANT = 317,
     NAME = 318,
     RENAME = 319,
     NAMEWARN = 320,
     EXTEND = 321,
     PRAGMA = 322,
     FEATURE = 323,
     VARARGS = 324,
     ENUM = 325,
     CLASS = 326,
     TYPENAME = 327,
     PRIVATE = 328,
     PUBLIC = 329,
     PROTECTED = 330,
     COLON = 331,
     STATIC = 332,
     VIRTUAL = 333,
     FRIEND = 334,
     THROW = 335,
     CATCH = 336,
     EXPLICIT = 337,
     STATIC_ASSERT = 338,
     CONSTEXPR = 339,
     THREAD_LOCAL = 340,
     DECLTYPE = 341,
     AUTO = 342,
     NOEXCEPT = 343,
     OVERRIDE = 344,
     FINAL = 345,
     USING = 346,
     NAMESPACE = 347,
     NATIVE = 348,
     INLINE = 349,
     TYPEMAP = 350,
     EXCEPT = 351,
     ECHO = 352,
     APPLY = 353,
     CLEAR = 354,
     SWIGTEMPLATE = 355,
     FRAGMENT = 356,
     WARN = 357,
     LESSTHAN = 358,
     GREATERTHAN = 359,
     DELETE_KW = 360,
     DEFAULT = 361,
     LESSTHANOREQUALTO = 362,
     GREATERTHANOREQUALTO = 363,
     EQUALTO = 364,
     NOTEQUALTO = 365,
     ARROW = 366,
     QUESTIONMARK = 367,
     TYPES = 368,
     PARMS = 369,
     NONID = 370,
     DSTAR = 371,
     DCNOT = 372,
     TEMPLATE = 373,
     OPERATOR = 374,
     CONVERSIONOPERATOR = 375,
     PARSETYPE = 376,
     PARSEPARM = 377,
     PARSEPARMS = 378,
     CAST = 379,
     LOR = 380,
     LAND = 381,
     OR = 382,
     XOR = 383,
     AND = 384,
     RSHIFT = 385,
     LSHIFT = 386,
     MINUS = 387,
     PLUS = 388,
     MODULO = 389,
     SLASH = 390,
     STAR = 391,
     LNOT = 392,
     NOT = 393,
     UMINUS = 394,
     DCOLON = 395
   };
#endif
/* Tokens.  */
#define ID 258
#define HBLOCK 259
#define POUND 260
#define STRING 261
#define WSTRING 262
#define INCLUDE 263
#define IMPORT 264
#define INSERT 265
#define CHARCONST 266
#define WCHARCONST 267
#define NUM_INT 268
#define NUM_FLOAT 269
#define NUM_UNSIGNED 270
#define NUM_LONG 271
#define NUM_ULONG 272
#define NUM_LONGLONG 273
#define NUM_ULONGLONG 274
#define NUM_BOOL 275
#define TYPEDEF 276
#define TYPE_INT 277
#define TYPE_UNSIGNED 278
#define TYPE_SHORT 279
#define TYPE_LONG 280
#define TYPE_FLOAT 281
#define TYPE_DOUBLE 282
#define TYPE_CHAR 283
#define TYPE_WCHAR 284
#define TYPE_VOID 285
#define TYPE_SIGNED 286
#define TYPE_BOOL 287
#define TYPE_COMPLEX 288
#define TYPE_TYPEDEF 289
#define TYPE_RAW 290
#define TYPE_NON_ISO_INT8 291
#define TYPE_NON_ISO_INT16 292
#define TYPE_NON_ISO_INT32 293
#define TYPE_NON_ISO_INT64 294
#define LPAREN 295
#define RPAREN 296
#define COMMA 297
#define SEMI 298
#define EXTERN 299
#define INIT 300
#define LBRACE 301
#define RBRACE 302
#define PERIOD 303
#define CONST_QUAL 304
#define VOLATILE 305
#define REGISTER 306
#define STRUCT 307
#define UNION 308
#define EQUAL 309
#define SIZEOF 310
#define MODULE 311
#define LBRACKET 312
#define RBRACKET 313
#define BEGINFILE 314
#define ENDOFFILE 315
#define ILLEGAL 316
#define CONSTANT 317
#define NAME 318
#define RENAME 319
#define NAMEWARN 320
#define EXTEND 321
#define PRAGMA 322
#define FEATURE 323
#define VARARGS 324
#define ENUM 325
#define CLASS 326
#define TYPENAME 327
#define PRIVATE 328
#define PUBLIC 329
#define PROTECTED 330
#define COLON 331
#define STATIC 332
#define VIRTUAL 333
#define FRIEND 334
#define THROW 335
#define CATCH 336
#define EXPLICIT 337
#define STATIC_ASSERT 338
#define CONSTEXPR 339
#define THREAD_LOCAL 340
#define DECLTYPE 341
#define AUTO 342
#define NOEXCEPT 343
#define OVERRIDE 344
#define FINAL 345
#define USING 346
#define NAMESPACE 347
#define NATIVE 348
#define INLINE 349
#define TYPEMAP 350
#define EXCEPT 351
#define ECHO 352
#define APPLY 353
#define CLEAR 354
#define SWIGTEMPLATE 355
#define FRAGMENT 356
#define WARN 357
#define LESSTHAN 358
#define GREATERTHAN 359
#define DELETE_KW 360
#define DEFAULT 361
#define LESSTHANOREQUALTO 362
#define GREATERTHANOREQUALTO 363
#define EQUALTO 364
#define NOTEQUALTO 365
#define ARROW 366
#define QUESTIONMARK 367
#define TYPES 368
#define PARMS 369
#define NONID 370
#define DSTAR 371
#define DCNOT 372
#define TEMPLATE 373
#define OPERATOR 374
#define CONVERSIONOPERATOR 375
#define PARSETYPE 376
#define PARSEPARM 377
#define PARSEPARMS 378
#define CAST 379
#define LOR 380
#define LAND 381
#define OR 382
#define XOR 383
#define AND 384
#define RSHIFT 385
#define LSHIFT 386
#define MINUS 387
#define PLUS 388
#define MODULO 389
#define SLASH 390
#define STAR 391
#define LNOT 392
#define NOT 393
#define UMINUS 394
#define DCOLON 395




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 1346 "parser.y"

  const char  *id;
  List  *bases;
  struct Define {
    String *val;
    String *rawval;
    int     type;
    String *qualifier;
    String *bitfield;
    Parm   *throws;
    String *throwf;
    String *nexcept;
  } dtype;
  struct {
    const char *type;
    String *filename;
    int   line;
  } loc;
  struct {
    char      *id;
    SwigType  *type;
    String    *defarg;
    ParmList  *parms;
    short      have_parms;
    ParmList  *throws;
    String    *throwf;
    String    *nexcept;
  } decl;
  Parm         *tparms;
  struct {
    String     *method;
    Hash       *kwargs;
  } tmap;
  struct {
    String     *type;
    String     *us;
  } ptype;
  SwigType     *type;
  String       *str;
  Parm         *p;
  ParmList     *pl;
  int           intvalue;
  Node         *node;



/* Line 214 of yacc.c  */
#line 1757 "CParse/parser.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 1769 "CParse/parser.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  60
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   5111

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  141
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  169
/* YYNRULES -- Number of rules.  */
#define YYNRULES  574
/* YYNRULES -- Number of states.  */
#define YYNSTATES  1122

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   395

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     9,    12,    16,    19,    25,    29,
      32,    34,    36,    38,    40,    42,    44,    46,    49,    51,
      53,    55,    57,    59,    61,    63,    65,    67,    69,    71,
      73,    75,    77,    79,    81,    83,    85,    87,    89,    91,
      92,   100,   106,   110,   116,   122,   126,   129,   132,   138,
     141,   147,   150,   155,   157,   159,   167,   175,   181,   182,
     190,   192,   194,   197,   200,   202,   208,   214,   220,   224,
     229,   233,   241,   250,   256,   260,   262,   264,   268,   270,
     275,   283,   290,   292,   294,   302,   312,   321,   332,   338,
     346,   353,   362,   364,   366,   372,   377,   383,   391,   393,
     397,   404,   411,   420,   422,   425,   429,   431,   434,   438,
     445,   451,   461,   464,   466,   468,   470,   471,   478,   480,
     487,   498,   504,   512,   514,   519,   521,   523,   525,   528,
     531,   535,   537,   539,   541,   543,   545,   547,   559,   573,
     581,   583,   585,   587,   588,   592,   594,   597,   600,   603,
     605,   611,   620,   631,   638,   640,   642,   644,   646,   648,
     650,   651,   661,   662,   671,   673,   677,   682,   683,   690,
     694,   699,   701,   703,   705,   707,   709,   711,   713,   715,
     718,   720,   722,   724,   728,   730,   734,   739,   740,   747,
     748,   754,   760,   763,   764,   765,   773,   775,   777,   778,
     782,   784,   786,   788,   790,   792,   794,   796,   798,   800,
     804,   806,   808,   810,   812,   814,   816,   818,   820,   822,
     829,   836,   844,   853,   862,   871,   881,   889,   895,   898,
     901,   904,   907,   909,   911,   913,   915,   917,   919,   921,
     923,   925,   927,   929,   932,   937,   940,   943,   948,   951,
     957,   959,   961,   963,   965,   967,   970,   972,   974,   977,
     980,   982,   984,   986,   988,   990,   992,   995,   998,  1001,
    1004,  1006,  1009,  1012,  1015,  1018,  1020,  1022,  1025,  1027,
    1031,  1033,  1036,  1044,  1048,  1050,  1053,  1055,  1059,  1061,
    1063,  1065,  1068,  1074,  1077,  1080,  1082,  1085,  1088,  1090,
    1092,  1094,  1096,  1099,  1103,  1107,  1109,  1112,  1115,  1119,
    1124,  1130,  1135,  1141,  1148,  1155,  1160,  1166,  1172,  1179,
    1187,  1196,  1205,  1213,  1221,  1223,  1226,  1230,  1235,  1241,
    1245,  1250,  1255,  1257,  1260,  1265,  1270,  1275,  1281,  1285,
    1290,  1295,  1301,  1303,  1306,  1309,  1312,  1316,  1320,  1322,
    1325,  1328,  1330,  1332,  1335,  1339,  1344,  1348,  1353,  1356,
    1360,  1364,  1369,  1373,  1377,  1380,  1383,  1385,  1387,  1390,
    1392,  1394,  1396,  1398,  1401,  1403,  1406,  1410,  1412,  1414,
    1416,  1419,  1421,  1423,  1426,  1428,  1433,  1435,  1437,  1440,
    1442,  1444,  1446,  1448,  1450,  1452,  1454,  1456,  1458,  1460,
    1462,  1464,  1466,  1468,  1469,  1472,  1474,  1476,  1478,  1480,
    1482,  1484,  1486,  1488,  1490,  1496,  1500,  1504,  1506,  1508,
    1512,  1514,  1516,  1518,  1520,  1522,  1528,  1537,  1539,  1541,
    1543,  1545,  1549,  1554,  1560,  1566,  1572,  1579,  1586,  1589,
    1592,  1595,  1597,  1599,  1601,  1603,  1605,  1607,  1609,  1611,
    1615,  1619,  1623,  1627,  1631,  1635,  1639,  1643,  1647,  1651,
    1655,  1659,  1663,  1667,  1671,  1675,  1681,  1684,  1687,  1690,
    1693,  1696,  1700,  1702,  1704,  1706,  1707,  1711,  1713,  1715,
    1719,  1720,  1725,  1726,  1733,  1735,  1737,  1739,  1741,  1743,
    1748,  1753,  1755,  1757,  1759,  1761,  1763,  1765,  1767,  1770,
    1773,  1778,  1780,  1782,  1785,  1790,  1792,  1794,  1797,  1799,
    1803,  1807,  1812,  1817,  1821,  1826,  1829,  1831,  1833,  1837,
    1842,  1849,  1852,  1855,  1859,  1861,  1863,  1865,  1867,  1869,
    1871,  1873,  1875,  1878,  1883,  1885,  1889,  1891,  1894,  1898,
    1902,  1905,  1908,  1911,  1913,  1916,  1918,  1922,  1925,  1930,
    1932,  1936,  1938,  1942,  1946,  1949,  1952,  1955,  1958,  1960,
    1963,  1965,  1967,  1969,  1971,  1975,  1977,  1981,  1987,  1989,
    1993,  1997,  2003,  2005,  2007
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     142,     0,    -1,   143,    -1,   121,   236,    43,    -1,   121,
       1,    -1,   122,   236,    43,    -1,   122,     1,    -1,   123,
      40,   233,    41,    43,    -1,   123,     1,    43,    -1,   143,
     144,    -1,   309,    -1,   145,    -1,   182,    -1,   198,    -1,
      43,    -1,     1,    -1,   197,    -1,     1,   120,    -1,   146,
      -1,   148,    -1,   149,    -1,   150,    -1,   151,    -1,   152,
      -1,   155,    -1,   156,    -1,   159,    -1,   160,    -1,   161,
      -1,   162,    -1,   163,    -1,   164,    -1,   167,    -1,   169,
      -1,   172,    -1,   174,    -1,   179,    -1,   180,    -1,   181,
      -1,    -1,    66,   306,   297,    46,   147,   215,    47,    -1,
      98,   178,    46,   176,    47,    -1,    99,   176,    43,    -1,
      62,   294,    54,   259,    43,    -1,    62,   252,   244,   241,
      43,    -1,    62,     1,    43,    -1,    97,     4,    -1,    97,
     303,    -1,    96,    40,   294,    41,    46,    -1,    96,    46,
      -1,    96,    40,   294,    41,    43,    -1,    96,    43,    -1,
     303,    46,   236,    47,    -1,   303,    -1,   153,    -1,   101,
      40,   154,    42,   307,    41,     4,    -1,   101,    40,   154,
      42,   307,    41,    46,    -1,   101,    40,   154,    41,    43,
      -1,    -1,   158,   306,   303,    59,   157,   143,    60,    -1,
       8,    -1,     9,    -1,    94,     4,    -1,    94,    46,    -1,
       4,    -1,    10,    40,   295,    41,   303,    -1,    10,    40,
     295,    41,     4,    -1,    10,    40,   295,    41,    46,    -1,
      56,   306,   295,    -1,    63,    40,   295,    41,    -1,    63,
      40,    41,    -1,    93,    40,   294,    41,   232,   294,    43,
      -1,    93,    40,   294,    41,   232,   252,   244,    43,    -1,
      67,   166,   294,    54,   165,    -1,    67,   166,   294,    -1,
     303,    -1,     4,    -1,    40,   294,    41,    -1,   309,    -1,
     168,   244,   295,    43,    -1,   168,    40,   307,    41,   244,
     288,    43,    -1,   168,    40,   307,    41,   303,    43,    -1,
      64,    -1,    65,    -1,    68,    40,   295,    41,   244,   288,
     170,    -1,    68,    40,   295,    42,   308,    41,   244,   288,
      43,    -1,    68,    40,   295,   171,    41,   244,   288,   170,
      -1,    68,    40,   295,    42,   308,   171,    41,   244,   288,
      43,    -1,    68,    40,   295,    41,   170,    -1,    68,    40,
     295,    42,   308,    41,    43,    -1,    68,    40,   295,   171,
      41,   170,    -1,    68,    40,   295,    42,   308,   171,    41,
      43,    -1,   305,    -1,    43,    -1,   114,    40,   233,    41,
      43,    -1,    42,   295,    54,   308,    -1,    42,   295,    54,
     308,   171,    -1,    69,    40,   173,    41,   244,   288,    43,
      -1,   233,    -1,    13,    42,   236,    -1,    95,    40,   175,
      41,   176,   305,    -1,    95,    40,   175,    41,   176,    43,
      -1,    95,    40,   175,    41,   176,    54,   178,    43,    -1,
     307,    -1,   178,   177,    -1,    42,   178,   177,    -1,   309,
      -1,   252,   243,    -1,    40,   233,    41,    -1,    40,   233,
      41,    40,   233,    41,    -1,   113,    40,   233,    41,   170,
      -1,   100,    40,   296,    41,   301,   103,   237,   104,    43,
      -1,   102,   303,    -1,   184,    -1,   196,    -1,   195,    -1,
      -1,    44,   303,    46,   183,   143,    47,    -1,   188,    -1,
      91,   297,    54,   252,   243,    43,    -1,   118,   103,   207,
     104,    91,   297,    54,   252,   243,    43,    -1,   232,   252,
     244,   186,   185,    -1,   232,    87,   244,   111,   187,   186,
     185,    -1,    43,    -1,    42,   244,   186,   185,    -1,    46,
      -1,     1,    -1,   241,    -1,   250,   241,    -1,   287,   241,
      -1,   250,   287,   241,    -1,   256,    -1,    32,    -1,    30,
      -1,    35,    -1,   297,    -1,   255,    -1,   232,    87,   297,
      54,   189,    40,   233,    41,   288,   190,   191,    -1,   232,
      87,   297,    54,   189,    40,   233,    41,   288,   111,   252,
     190,   191,    -1,   232,    87,   297,    54,   189,   190,   191,
      -1,    57,    -1,    46,    -1,    43,    -1,    -1,    40,   192,
      43,    -1,    70,    -1,    70,    71,    -1,    70,    52,    -1,
      76,   254,    -1,   309,    -1,   232,   193,   264,   194,    43,
      -1,   232,   193,   264,   194,    46,   266,    47,    43,    -1,
     232,   193,   264,   194,    46,   266,    47,   244,   186,   185,
      -1,   232,   252,    40,   233,    41,   289,    -1,   199,    -1,
     203,    -1,   204,    -1,   211,    -1,   212,    -1,   223,    -1,
      -1,   232,   284,   297,   275,    46,   200,   215,    47,   202,
      -1,    -1,   232,   284,   275,    46,   201,   215,    47,   202,
      -1,    43,    -1,   244,   186,   185,    -1,   232,   284,   297,
      43,    -1,    -1,   118,   103,   207,   104,   205,   206,    -1,
     118,   284,   297,    -1,    44,   118,   284,   297,    -1,   184,
      -1,   199,    -1,   220,    -1,   224,    -1,   204,    -1,   203,
      -1,   222,    -1,   208,    -1,   209,   210,    -1,   309,    -1,
     283,    -1,   236,    -1,    42,   209,   210,    -1,   309,    -1,
      91,   297,    43,    -1,    91,    92,   297,    43,    -1,    -1,
      92,   297,    46,   213,   143,    47,    -1,    -1,    92,    46,
     214,   143,    47,    -1,    92,   294,    54,   297,    43,    -1,
     219,   215,    -1,    -1,    -1,    66,    46,   216,   215,    47,
     217,   215,    -1,   156,    -1,   309,    -1,    -1,     1,   218,
     215,    -1,   182,    -1,   220,    -1,   221,    -1,   224,    -1,
     225,    -1,   226,    -1,   222,    -1,   203,    -1,   199,    -1,
     232,   297,    43,    -1,   211,    -1,   204,    -1,   223,    -1,
     180,    -1,   181,    -1,   229,    -1,   155,    -1,   179,    -1,
      43,    -1,   232,   252,    40,   233,    41,   289,    -1,   138,
     299,    40,   233,    41,   227,    -1,    78,   138,   299,    40,
     233,    41,   228,    -1,   232,   120,   252,   249,    40,   233,
      41,   228,    -1,   232,   120,   252,   129,    40,   233,    41,
     228,    -1,   232,   120,   252,   126,    40,   233,    41,   228,
      -1,   232,   120,   252,   249,   129,    40,   233,    41,   228,
      -1,   232,   120,   252,    40,   233,    41,   228,    -1,    81,
      40,   233,    41,    46,    -1,    83,    40,    -1,    74,    76,
      -1,    73,    76,    -1,    75,    76,    -1,   164,    -1,   150,
      -1,   162,    -1,   167,    -1,   169,    -1,   172,    -1,   160,
      -1,   174,    -1,   148,    -1,   149,    -1,   151,    -1,   288,
      43,    -1,   288,    54,   261,    43,    -1,   288,    46,    -1,
     288,    43,    -1,   288,    54,   259,    43,    -1,   288,    46,
      -1,   232,   230,    76,   269,    43,    -1,   256,    -1,    32,
      -1,    30,    -1,    35,    -1,   297,    -1,    44,   303,    -1,
      44,    -1,   231,    -1,   231,    85,    -1,   231,    21,    -1,
      77,    -1,    21,    -1,    78,    -1,    79,    -1,    82,    -1,
      84,    -1,    82,    84,    -1,    84,    82,    -1,    77,    84,
      -1,    84,    77,    -1,    85,    -1,    85,    77,    -1,    77,
      85,    -1,    44,    85,    -1,    85,    44,    -1,   309,    -1,
     234,    -1,   236,   235,    -1,   309,    -1,    42,   236,   235,
      -1,   309,    -1,   253,   242,    -1,   118,   103,   284,   104,
     284,   297,   241,    -1,    48,    48,    48,    -1,   238,    -1,
     240,   239,    -1,   309,    -1,    42,   240,   239,    -1,   309,
      -1,   236,    -1,   270,    -1,    54,   259,    -1,    54,   259,
      57,   269,    58,    -1,    54,    46,    -1,    76,   269,    -1,
     309,    -1,   244,   241,    -1,   247,   241,    -1,   241,    -1,
     244,    -1,   247,    -1,   309,    -1,   249,   245,    -1,   249,
     129,   245,    -1,   249,   126,   245,    -1,   246,    -1,   129,
     245,    -1,   126,   245,    -1,   297,   116,   245,    -1,   249,
     297,   116,   245,    -1,   249,   297,   116,   129,   245,    -1,
     297,   116,   129,   245,    -1,   249,    48,    48,    48,   245,
      -1,   249,   129,    48,    48,    48,   245,    -1,   249,   126,
      48,    48,    48,   245,    -1,    48,    48,    48,   246,    -1,
     129,    48,    48,    48,   245,    -1,   126,    48,    48,    48,
     245,    -1,   297,   116,    48,    48,    48,   245,    -1,   249,
     297,   116,    48,    48,    48,   245,    -1,   249,   297,   116,
     129,    48,    48,    48,   245,    -1,   249,   297,   116,   126,
      48,    48,    48,   245,    -1,   297,   116,   129,    48,    48,
      48,   245,    -1,   297,   116,   126,    48,    48,    48,   245,
      -1,   297,    -1,   138,   297,    -1,    40,   297,    41,    -1,
      40,   249,   245,    41,    -1,    40,   297,   116,   245,    41,
      -1,   245,    57,    58,    -1,   245,    57,   269,    58,    -1,
     245,    40,   233,    41,    -1,   297,    -1,   138,   297,    -1,
      40,   249,   246,    41,    -1,    40,   129,   246,    41,    -1,
      40,   126,   246,    41,    -1,    40,   297,   116,   246,    41,
      -1,   246,    57,    58,    -1,   246,    57,   269,    58,    -1,
     246,    40,   233,    41,    -1,   119,     3,    40,   233,    41,
      -1,   249,    -1,   249,   248,    -1,   249,   129,    -1,   249,
     126,    -1,   249,   129,   248,    -1,   249,   126,   248,    -1,
     248,    -1,   129,   248,    -1,   126,   248,    -1,   129,    -1,
     126,    -1,   297,   116,    -1,   249,   297,   116,    -1,   249,
     297,   116,   248,    -1,   248,    57,    58,    -1,   248,    57,
     269,    58,    -1,    57,    58,    -1,    57,   269,    58,    -1,
      40,   247,    41,    -1,   248,    40,   233,    41,    -1,    40,
     233,    41,    -1,   136,   250,   249,    -1,   136,   249,    -1,
     136,   250,    -1,   136,    -1,   251,    -1,   251,   250,    -1,
      49,    -1,    50,    -1,    51,    -1,   253,    -1,   250,   254,
      -1,   254,    -1,   254,   250,    -1,   250,   254,   250,    -1,
     256,    -1,    32,    -1,    30,    -1,   193,   297,    -1,    35,
      -1,   297,    -1,   284,   297,    -1,   255,    -1,    86,    40,
     297,    41,    -1,   257,    -1,   258,    -1,   258,   257,    -1,
      22,    -1,    24,    -1,    25,    -1,    28,    -1,    29,    -1,
      26,    -1,    27,    -1,    31,    -1,    23,    -1,    33,    -1,
      36,    -1,    37,    -1,    38,    -1,    39,    -1,    -1,   260,
     269,    -1,   261,    -1,   262,    -1,   263,    -1,   105,    -1,
     106,    -1,   294,    -1,   309,    -1,   150,    -1,   309,    -1,
     266,    42,   265,   267,   265,    -1,   266,    42,   265,    -1,
     265,   267,   265,    -1,   265,    -1,   294,    -1,   294,    54,
     268,    -1,   269,    -1,   270,    -1,   252,    -1,   271,    -1,
     303,    -1,    55,    40,   252,   242,    41,    -1,    55,    48,
      48,    48,    40,   252,   242,    41,    -1,   272,    -1,   304,
      -1,    11,    -1,    12,    -1,    40,   269,    41,    -1,    40,
     269,    41,   269,    -1,    40,   269,   249,    41,   269,    -1,
      40,   269,   129,    41,   269,    -1,    40,   269,   126,    41,
     269,    -1,    40,   269,   249,   129,    41,   269,    -1,    40,
     269,   249,   126,    41,   269,    -1,   129,   269,    -1,   126,
     269,    -1,   136,   269,    -1,    13,    -1,    14,    -1,    15,
      -1,    16,    -1,    17,    -1,    18,    -1,    19,    -1,    20,
      -1,   269,   133,   269,    -1,   269,   132,   269,    -1,   269,
     136,   269,    -1,   269,   135,   269,    -1,   269,   134,   269,
      -1,   269,   129,   269,    -1,   269,   127,   269,    -1,   269,
     128,   269,    -1,   269,   131,   269,    -1,   269,   130,   269,
      -1,   269,   126,   269,    -1,   269,   125,   269,    -1,   269,
     109,   269,    -1,   269,   110,   269,    -1,   269,   108,   269,
      -1,   269,   107,   269,    -1,   269,   112,   269,    76,   269,
      -1,   132,   269,    -1,   133,   269,    -1,   138,   269,    -1,
     137,   269,    -1,   252,    40,    -1,    48,    48,    48,    -1,
     273,    -1,   309,    -1,   276,    -1,    -1,    76,   277,   278,
      -1,   309,    -1,   279,    -1,   278,    42,   279,    -1,    -1,
     285,   280,   297,   274,    -1,    -1,   285,   282,   281,   285,
     297,   274,    -1,    74,    -1,    73,    -1,    75,    -1,    71,
      -1,    72,    -1,    71,    48,    48,    48,    -1,    72,    48,
      48,    48,    -1,   283,    -1,    52,    -1,    53,    -1,    78,
      -1,   309,    -1,    89,    -1,    90,    -1,    90,    89,    -1,
      89,    90,    -1,    80,    40,   233,    41,    -1,    88,    -1,
     286,    -1,    88,   286,    -1,    88,    40,   269,    41,    -1,
     250,    -1,   287,    -1,   250,   287,    -1,   309,    -1,   288,
     290,    43,    -1,   288,   290,    46,    -1,    40,   233,    41,
      43,    -1,    40,   233,    41,    46,    -1,    54,   259,    43,
      -1,   287,    54,   261,    43,    -1,    76,   291,    -1,   309,
      -1,   292,    -1,   291,    42,   292,    -1,   292,    48,    48,
      48,    -1,   291,    42,   292,    48,    48,    48,    -1,   297,
      40,    -1,   297,    46,    -1,   103,   237,   104,    -1,     3,
      -1,    89,    -1,    90,    -1,   294,    -1,   261,    -1,   303,
      -1,   295,    -1,   309,    -1,   299,   298,    -1,   115,   140,
     300,   298,    -1,   299,    -1,   115,   140,   300,    -1,   119,
      -1,   119,   293,    -1,   115,   140,   119,    -1,   140,   300,
     298,    -1,   140,   300,    -1,   140,   119,    -1,   117,   299,
      -1,   294,    -1,   294,   293,    -1,   299,    -1,   118,   294,
     293,    -1,   294,   302,    -1,   115,   140,   294,   302,    -1,
     294,    -1,   115,   140,   294,    -1,   119,    -1,   115,   140,
     119,    -1,   140,   294,   302,    -1,   140,   294,    -1,   140,
     119,    -1,   117,   294,    -1,   303,     6,    -1,     6,    -1,
     304,     7,    -1,     7,    -1,   303,    -1,    46,    -1,     4,
      -1,    40,   307,    41,    -1,   309,    -1,   295,    54,   308,
      -1,   295,    54,   308,    42,   307,    -1,   295,    -1,   295,
      42,   307,    -1,   295,    54,   153,    -1,   295,    54,   153,
      42,   307,    -1,   303,    -1,   271,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1511,  1511,  1523,  1527,  1530,  1533,  1536,  1539,  1544,
    1549,  1554,  1555,  1556,  1557,  1558,  1568,  1584,  1594,  1595,
    1596,  1597,  1598,  1599,  1600,  1601,  1602,  1603,  1604,  1605,
    1606,  1607,  1608,  1609,  1610,  1611,  1612,  1613,  1614,  1621,
    1621,  1703,  1713,  1724,  1745,  1767,  1778,  1787,  1806,  1812,
    1818,  1823,  1830,  1837,  1841,  1854,  1863,  1878,  1891,  1891,
    1947,  1948,  1955,  1974,  2005,  2009,  2019,  2024,  2042,  2085,
    2091,  2104,  2110,  2136,  2142,  2149,  2150,  2153,  2154,  2161,
    2207,  2253,  2264,  2267,  2294,  2300,  2306,  2312,  2320,  2326,
    2332,  2338,  2346,  2347,  2348,  2351,  2356,  2366,  2402,  2403,
    2438,  2455,  2463,  2476,  2501,  2507,  2511,  2514,  2525,  2530,
    2543,  2555,  2853,  2863,  2870,  2871,  2875,  2875,  2900,  2906,
    2916,  2934,  3002,  3060,  3064,  3087,  3091,  3102,  3109,  3116,
    3123,  3132,  3133,  3134,  3138,  3139,  3140,  3151,  3156,  3161,
    3168,  3174,  3179,  3182,  3182,  3195,  3198,  3201,  3210,  3213,
    3220,  3242,  3271,  3369,  3421,  3422,  3423,  3424,  3425,  3426,
    3431,  3431,  3679,  3679,  3826,  3827,  3839,  3857,  3857,  4116,
    4122,  4128,  4131,  4134,  4137,  4140,  4143,  4146,  4151,  4187,
    4191,  4194,  4197,  4202,  4206,  4211,  4221,  4252,  4252,  4281,
    4281,  4303,  4330,  4347,  4352,  4347,  4360,  4361,  4362,  4362,
    4378,  4379,  4396,  4397,  4398,  4399,  4400,  4401,  4402,  4403,
    4404,  4405,  4406,  4407,  4408,  4409,  4410,  4411,  4412,  4421,
    4449,  4476,  4507,  4522,  4539,  4557,  4576,  4595,  4602,  4609,
    4616,  4624,  4632,  4635,  4639,  4642,  4643,  4644,  4645,  4646,
    4647,  4648,  4649,  4652,  4659,  4666,  4675,  4684,  4693,  4705,
    4708,  4711,  4712,  4716,  4718,  4726,  4738,  4739,  4740,  4741,
    4742,  4743,  4744,  4745,  4746,  4747,  4748,  4749,  4750,  4751,
    4752,  4753,  4754,  4755,  4756,  4757,  4764,  4775,  4779,  4782,
    4786,  4790,  4800,  4808,  4816,  4829,  4833,  4836,  4840,  4844,
    4872,  4880,  4892,  4907,  4917,  4926,  4937,  4941,  4945,  4952,
    4969,  4986,  4994,  5002,  5011,  5020,  5024,  5033,  5044,  5055,
    5067,  5077,  5091,  5099,  5108,  5117,  5121,  5130,  5141,  5152,
    5164,  5174,  5184,  5195,  5208,  5215,  5223,  5239,  5247,  5258,
    5269,  5280,  5299,  5307,  5324,  5332,  5339,  5346,  5357,  5368,
    5379,  5399,  5420,  5426,  5432,  5439,  5446,  5455,  5464,  5467,
    5476,  5485,  5492,  5499,  5506,  5516,  5527,  5538,  5549,  5556,
    5563,  5566,  5583,  5593,  5600,  5606,  5611,  5617,  5621,  5627,
    5628,  5629,  5635,  5641,  5645,  5646,  5650,  5657,  5660,  5661,
    5665,  5666,  5668,  5671,  5674,  5679,  5690,  5715,  5718,  5772,
    5776,  5780,  5784,  5788,  5792,  5796,  5800,  5804,  5808,  5812,
    5816,  5820,  5824,  5830,  5830,  5844,  5849,  5852,  5858,  5871,
    5885,  5886,  5889,  5890,  5894,  5900,  5903,  5907,  5912,  5920,
    5932,  5947,  5948,  5967,  5968,  5972,  5977,  5982,  5983,  5988,
    6001,  6016,  6023,  6040,  6047,  6054,  6061,  6069,  6077,  6081,
    6085,  6091,  6092,  6093,  6094,  6095,  6096,  6097,  6098,  6101,
    6105,  6109,  6113,  6117,  6121,  6125,  6129,  6133,  6137,  6141,
    6145,  6149,  6153,  6167,  6171,  6175,  6181,  6185,  6189,  6193,
    6197,  6213,  6218,  6221,  6226,  6231,  6231,  6232,  6235,  6252,
    6261,  6261,  6279,  6279,  6297,  6298,  6299,  6303,  6307,  6311,
    6315,  6321,  6324,  6328,  6334,  6335,  6338,  6341,  6344,  6347,
    6352,  6357,  6362,  6367,  6372,  6379,  6385,  6389,  6393,  6401,
    6409,  6417,  6426,  6435,  6442,  6451,  6452,  6455,  6456,  6457,
    6458,  6461,  6473,  6479,  6488,  6489,  6490,  6493,  6494,  6495,
    6498,  6499,  6502,  6507,  6511,  6514,  6517,  6520,  6523,  6528,
    6532,  6535,  6542,  6548,  6551,  6556,  6559,  6565,  6570,  6574,
    6577,  6580,  6583,  6588,  6592,  6595,  6598,  6604,  6607,  6610,
    6618,  6621,  6624,  6628,  6633,  6646,  6650,  6655,  6661,  6665,
    6670,  6674,  6681,  6684,  6689
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ID", "HBLOCK", "POUND", "STRING",
  "WSTRING", "INCLUDE", "IMPORT", "INSERT", "CHARCONST", "WCHARCONST",
  "NUM_INT", "NUM_FLOAT", "NUM_UNSIGNED", "NUM_LONG", "NUM_ULONG",
  "NUM_LONGLONG", "NUM_ULONGLONG", "NUM_BOOL", "TYPEDEF", "TYPE_INT",
  "TYPE_UNSIGNED", "TYPE_SHORT", "TYPE_LONG", "TYPE_FLOAT", "TYPE_DOUBLE",
  "TYPE_CHAR", "TYPE_WCHAR", "TYPE_VOID", "TYPE_SIGNED", "TYPE_BOOL",
  "TYPE_COMPLEX", "TYPE_TYPEDEF", "TYPE_RAW", "TYPE_NON_ISO_INT8",
  "TYPE_NON_ISO_INT16", "TYPE_NON_ISO_INT32", "TYPE_NON_ISO_INT64",
  "LPAREN", "RPAREN", "COMMA", "SEMI", "EXTERN", "INIT", "LBRACE",
  "RBRACE", "PERIOD", "CONST_QUAL", "VOLATILE", "REGISTER", "STRUCT",
  "UNION", "EQUAL", "SIZEOF", "MODULE", "LBRACKET", "RBRACKET",
  "BEGINFILE", "ENDOFFILE", "ILLEGAL", "CONSTANT", "NAME", "RENAME",
  "NAMEWARN", "EXTEND", "PRAGMA", "FEATURE", "VARARGS", "ENUM", "CLASS",
  "TYPENAME", "PRIVATE", "PUBLIC", "PROTECTED", "COLON", "STATIC",
  "VIRTUAL", "FRIEND", "THROW", "CATCH", "EXPLICIT", "STATIC_ASSERT",
  "CONSTEXPR", "THREAD_LOCAL", "DECLTYPE", "AUTO", "NOEXCEPT", "OVERRIDE",
  "FINAL", "USING", "NAMESPACE", "NATIVE", "INLINE", "TYPEMAP", "EXCEPT",
  "ECHO", "APPLY", "CLEAR", "SWIGTEMPLATE", "FRAGMENT", "WARN", "LESSTHAN",
  "GREATERTHAN", "DELETE_KW", "DEFAULT", "LESSTHANOREQUALTO",
  "GREATERTHANOREQUALTO", "EQUALTO", "NOTEQUALTO", "ARROW", "QUESTIONMARK",
  "TYPES", "PARMS", "NONID", "DSTAR", "DCNOT", "TEMPLATE", "OPERATOR",
  "CONVERSIONOPERATOR", "PARSETYPE", "PARSEPARM", "PARSEPARMS", "CAST",
  "LOR", "LAND", "OR", "XOR", "AND", "RSHIFT", "LSHIFT", "MINUS", "PLUS",
  "MODULO", "SLASH", "STAR", "LNOT", "NOT", "UMINUS", "DCOLON", "$accept",
  "program", "interface", "declaration", "swig_directive",
  "extend_directive", "$@1", "apply_directive", "clear_directive",
  "constant_directive", "echo_directive", "except_directive", "stringtype",
  "fname", "fragment_directive", "include_directive", "$@2", "includetype",
  "inline_directive", "insert_directive", "module_directive",
  "name_directive", "native_directive", "pragma_directive", "pragma_arg",
  "pragma_lang", "rename_directive", "rename_namewarn",
  "feature_directive", "stringbracesemi", "featattr", "varargs_directive",
  "varargs_parms", "typemap_directive", "typemap_type", "tm_list",
  "tm_tail", "typemap_parm", "types_directive", "template_directive",
  "warn_directive", "c_declaration", "$@3", "c_decl", "c_decl_tail",
  "initializer", "cpp_alternate_rettype", "cpp_lambda_decl",
  "lambda_introducer", "lambda_body", "lambda_tail", "$@4", "c_enum_key",
  "c_enum_inherit", "c_enum_forward_decl", "c_enum_decl",
  "c_constructor_decl", "cpp_declaration", "cpp_class_decl", "@5", "@6",
  "cpp_opt_declarators", "cpp_forward_class_decl", "cpp_template_decl",
  "$@7", "cpp_temp_possible", "template_parms", "templateparameters",
  "templateparameter", "templateparameterstail", "cpp_using_decl",
  "cpp_namespace_decl", "$@8", "$@9", "cpp_members", "$@10", "$@11",
  "$@12", "cpp_member", "cpp_constructor_decl", "cpp_destructor_decl",
  "cpp_conversion_operator", "cpp_catch_decl", "cpp_static_assert",
  "cpp_protection_decl", "cpp_swig_directive", "cpp_end", "cpp_vend",
  "anonymous_bitfield", "anon_bitfield_type", "extern_string",
  "storage_class", "parms", "rawparms", "ptail", "parm", "valparms",
  "rawvalparms", "valptail", "valparm", "def_args", "parameter_declarator",
  "plain_declarator", "declarator", "notso_direct_declarator",
  "direct_declarator", "abstract_declarator", "direct_abstract_declarator",
  "pointer", "type_qualifier", "type_qualifier_raw", "type", "rawtype",
  "type_right", "decltype", "primitive_type", "primitive_type_list",
  "type_specifier", "definetype", "$@13", "default_delete",
  "deleted_definition", "explicit_default", "ename",
  "optional_constant_directive", "enumlist", "edecl", "etype", "expr",
  "valexpr", "exprnum", "exprcompound", "ellipsis", "variadic", "inherit",
  "raw_inherit", "$@14", "base_list", "base_specifier", "@15", "@16",
  "access_specifier", "templcpptype", "cpptype", "opt_virtual",
  "virt_specifier_seq", "exception_specification", "cpp_const", "ctor_end",
  "ctor_initializer", "mem_initializer_list", "mem_initializer",
  "less_valparms_greater", "identifier", "idstring", "idstringopt",
  "idcolon", "idcolontail", "idtemplate", "idtemplatetemplate",
  "idcolonnt", "idcolontailnt", "string", "wstring", "stringbrace",
  "options", "kwargs", "stringnum", "empty", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   141,   142,   142,   142,   142,   142,   142,   142,   143,
     143,   144,   144,   144,   144,   144,   144,   144,   145,   145,
     145,   145,   145,   145,   145,   145,   145,   145,   145,   145,
     145,   145,   145,   145,   145,   145,   145,   145,   145,   147,
     146,   148,   149,   150,   150,   150,   151,   151,   152,   152,
     152,   152,   153,   154,   154,   155,   155,   155,   157,   156,
     158,   158,   159,   159,   160,   160,   160,   160,   161,   162,
     162,   163,   163,   164,   164,   165,   165,   166,   166,   167,
     167,   167,   168,   168,   169,   169,   169,   169,   169,   169,
     169,   169,   170,   170,   170,   171,   171,   172,   173,   173,
     174,   174,   174,   175,   176,   177,   177,   178,   178,   178,
     179,   180,   181,   182,   182,   182,   183,   182,   182,   182,
     182,   184,   184,   185,   185,   185,   185,   186,   186,   186,
     186,   187,   187,   187,   187,   187,   187,   188,   188,   188,
     189,   190,   191,   192,   191,   193,   193,   193,   194,   194,
     195,   196,   196,   197,   198,   198,   198,   198,   198,   198,
     200,   199,   201,   199,   202,   202,   203,   205,   204,   204,
     204,   206,   206,   206,   206,   206,   206,   206,   207,   208,
     208,   209,   209,   210,   210,   211,   211,   213,   212,   214,
     212,   212,   215,   216,   217,   215,   215,   215,   218,   215,
     219,   219,   219,   219,   219,   219,   219,   219,   219,   219,
     219,   219,   219,   219,   219,   219,   219,   219,   219,   220,
     221,   221,   222,   222,   222,   222,   222,   223,   224,   225,
     225,   225,   226,   226,   226,   226,   226,   226,   226,   226,
     226,   226,   226,   227,   227,   227,   228,   228,   228,   229,
     230,   230,   230,   230,   230,   231,   232,   232,   232,   232,
     232,   232,   232,   232,   232,   232,   232,   232,   232,   232,
     232,   232,   232,   232,   232,   232,   233,   234,   234,   235,
     235,   236,   236,   236,   237,   238,   238,   239,   239,   240,
     240,   241,   241,   241,   241,   241,   242,   242,   242,   243,
     243,   243,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   245,   245,   245,   245,   245,   245,
     245,   245,   246,   246,   246,   246,   246,   246,   246,   246,
     246,   246,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   248,   248,   248,   248,
     248,   248,   248,   249,   249,   249,   249,   250,   250,   251,
     251,   251,   252,   253,   253,   253,   253,   254,   254,   254,
     254,   254,   254,   254,   254,   255,   256,   257,   257,   258,
     258,   258,   258,   258,   258,   258,   258,   258,   258,   258,
     258,   258,   258,   260,   259,   259,   261,   261,   262,   263,
     264,   264,   265,   265,   266,   266,   266,   266,   267,   267,
     268,   269,   269,   270,   270,   270,   270,   270,   270,   270,
     270,   270,   270,   270,   270,   270,   270,   270,   270,   270,
     270,   271,   271,   271,   271,   271,   271,   271,   271,   272,
     272,   272,   272,   272,   272,   272,   272,   272,   272,   272,
     272,   272,   272,   272,   272,   272,   272,   272,   272,   272,
     272,   273,   274,   274,   275,   277,   276,   276,   278,   278,
     280,   279,   281,   279,   282,   282,   282,   283,   283,   283,
     283,   284,   284,   284,   285,   285,   286,   286,   286,   286,
     287,   287,   287,   287,   287,   288,   288,   288,   288,   289,
     289,   289,   289,   289,   289,   290,   290,   291,   291,   291,
     291,   292,   292,   293,   294,   294,   294,   295,   295,   295,
     296,   296,   297,   297,   297,   297,   297,   297,   297,   298,
     298,   298,   298,   299,   299,   300,   300,   301,   301,   301,
     301,   301,   301,   302,   302,   302,   302,   303,   303,   304,
     304,   305,   305,   305,   306,   306,   307,   307,   307,   307,
     307,   307,   308,   308,   309
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     3,     2,     3,     2,     5,     3,     2,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       7,     5,     3,     5,     5,     3,     2,     2,     5,     2,
       5,     2,     4,     1,     1,     7,     7,     5,     0,     7,
       1,     1,     2,     2,     1,     5,     5,     5,     3,     4,
       3,     7,     8,     5,     3,     1,     1,     3,     1,     4,
       7,     6,     1,     1,     7,     9,     8,    10,     5,     7,
       6,     8,     1,     1,     5,     4,     5,     7,     1,     3,
       6,     6,     8,     1,     2,     3,     1,     2,     3,     6,
       5,     9,     2,     1,     1,     1,     0,     6,     1,     6,
      10,     5,     7,     1,     4,     1,     1,     1,     2,     2,
       3,     1,     1,     1,     1,     1,     1,    11,    13,     7,
       1,     1,     1,     0,     3,     1,     2,     2,     2,     1,
       5,     8,    10,     6,     1,     1,     1,     1,     1,     1,
       0,     9,     0,     8,     1,     3,     4,     0,     6,     3,
       4,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     3,     1,     3,     4,     0,     6,     0,
       5,     5,     2,     0,     0,     7,     1,     1,     0,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     6,
       6,     7,     8,     8,     8,     9,     7,     5,     2,     2,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     4,     2,     2,     4,     2,     5,
       1,     1,     1,     1,     1,     2,     1,     1,     2,     2,
       1,     1,     1,     1,     1,     1,     2,     2,     2,     2,
       1,     2,     2,     2,     2,     1,     1,     2,     1,     3,
       1,     2,     7,     3,     1,     2,     1,     3,     1,     1,
       1,     2,     5,     2,     2,     1,     2,     2,     1,     1,
       1,     1,     2,     3,     3,     1,     2,     2,     3,     4,
       5,     4,     5,     6,     6,     4,     5,     5,     6,     7,
       8,     8,     7,     7,     1,     2,     3,     4,     5,     3,
       4,     4,     1,     2,     4,     4,     4,     5,     3,     4,
       4,     5,     1,     2,     2,     2,     3,     3,     1,     2,
       2,     1,     1,     2,     3,     4,     3,     4,     2,     3,
       3,     4,     3,     3,     2,     2,     1,     1,     2,     1,
       1,     1,     1,     2,     1,     2,     3,     1,     1,     1,
       2,     1,     1,     2,     1,     4,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     5,     3,     3,     1,     1,     3,
       1,     1,     1,     1,     1,     5,     8,     1,     1,     1,
       1,     3,     4,     5,     5,     5,     6,     6,     2,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     5,     2,     2,     2,     2,
       2,     3,     1,     1,     1,     0,     3,     1,     1,     3,
       0,     4,     0,     6,     1,     1,     1,     1,     1,     4,
       4,     1,     1,     1,     1,     1,     1,     1,     2,     2,
       4,     1,     1,     2,     4,     1,     1,     2,     1,     3,
       3,     4,     4,     3,     4,     2,     1,     1,     3,     4,
       6,     2,     2,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     4,     1,     3,     1,     2,     3,     3,
       2,     2,     2,     1,     2,     1,     3,     2,     4,     1,
       3,     1,     3,     3,     2,     2,     2,     2,     1,     2,
       1,     1,     1,     1,     3,     1,     3,     5,     1,     3,
       3,     5,     1,     1,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
     574,     0,     0,     0,     0,     0,    10,     4,   524,   389,
     397,   390,   391,   394,   395,   392,   393,   379,   396,   378,
     398,   381,   399,   400,   401,   402,     0,   369,   370,   371,
     492,   493,   145,   487,   488,     0,   525,   526,     0,     0,
     536,     0,     0,     0,   367,   574,   374,   384,   377,   386,
     387,   491,     0,   543,   382,   534,     6,     0,     0,   574,
       1,    15,    64,    60,    61,     0,   261,    14,   256,   574,
       0,     0,    82,    83,   574,   574,     0,     0,   260,   262,
     263,     0,   264,   265,   270,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     9,
      11,    18,    19,    20,    21,    22,    23,    24,    25,   574,
      26,    27,    28,    29,    30,    31,    32,     0,    33,    34,
      35,    36,    37,    38,    12,   113,   118,   115,   114,    16,
      13,   154,   155,   156,   157,   158,   159,   257,     0,   275,
       0,   147,   146,     0,     0,     0,     0,     0,   574,   537,
     380,     3,   373,   368,   574,     0,   403,     0,     0,   536,
     352,   351,   366,     0,   298,   281,   574,   305,   574,   348,
     342,   332,   295,   375,   388,   383,   544,     0,     0,   532,
       5,     8,     0,   276,   574,   278,    17,     0,   558,   273,
       0,   255,     0,     0,   565,     0,     0,   372,   543,     0,
       0,     0,     0,    78,     0,   574,   268,   272,   574,   266,
     269,   267,   274,   271,     0,     0,   189,   543,     0,     0,
      62,    63,     0,     0,    51,    49,    46,    47,   574,     0,
     574,     0,   574,   574,     0,   112,   574,   574,     0,     0,
       0,     0,     0,     0,     0,   332,   259,   258,     0,   574,
       0,   574,   283,     0,     0,     0,     0,   538,   545,   535,
       0,   560,   429,   430,   441,   442,   443,   444,   445,   446,
     447,   448,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   289,     0,   284,   574,   422,   372,     0,   421,   423,
     427,   424,   428,   286,   376,   574,   352,   351,     0,     0,
     342,   382,     0,   293,   408,   409,   291,     0,   405,   406,
     407,   358,     0,   421,   294,     0,   574,     0,     0,   307,
     350,   324,     0,   306,   349,   364,   365,   333,   296,   574,
       0,   297,   574,     0,     0,   345,   344,   302,   343,   324,
     353,   542,   541,   540,     0,     0,   277,   280,   528,   527,
       0,   529,     0,   557,   116,   568,     0,    68,    45,     0,
     574,   403,    70,     0,     0,     0,    74,     0,     0,     0,
      98,     0,     0,   185,     0,   574,     0,   187,     0,     0,
     103,     0,     0,     0,   107,   299,   300,   301,    42,     0,
     104,   106,   530,     0,   531,    54,     0,    53,     0,     0,
     178,   574,   182,   491,   180,   169,     0,     0,     0,     0,
     527,     0,     0,     0,     0,     0,     0,   324,     0,     0,
     332,   574,   543,   411,   574,   574,   475,     0,   474,   383,
     477,   489,   490,   385,     0,   533,     0,     0,     0,     0,
     439,   438,   466,   467,   440,   469,   468,   523,     0,   285,
     288,   470,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   559,
     352,   351,   342,   382,     0,   332,     0,   362,   360,   345,
     344,     0,   332,   353,     0,     0,   404,   359,   574,   342,
     382,     0,   325,   574,     0,     0,   363,     0,   338,     0,
       0,   356,     0,     0,     0,   304,   347,     0,   303,   346,
     354,     0,     0,     0,   308,   539,     7,   574,     0,   170,
     574,     0,     0,   564,     0,     0,    69,    39,    77,     0,
       0,     0,     0,     0,     0,     0,   186,   574,     0,     0,
     574,   574,     0,     0,   108,     0,   574,     0,     0,     0,
       0,     0,   167,     0,   179,   184,    58,     0,     0,     0,
       0,    79,     0,     0,     0,     0,     0,   149,     0,   382,
       0,   501,   496,   497,     0,   127,   574,   502,   574,   574,
     162,   166,     0,   546,     0,   431,     0,     0,   366,     0,
     574,     0,   574,   464,   463,   461,   462,     0,   460,   459,
     455,   456,   454,   458,   457,   450,   449,   453,   452,   451,
       0,   353,   336,   335,   334,   354,     0,   315,     0,     0,
       0,   324,   326,   353,     0,     0,   329,     0,     0,   340,
     339,   361,   357,     0,     0,     0,     0,     0,     0,   309,
     355,     0,     0,     0,   311,   279,    66,    67,    65,     0,
     569,   570,   573,   572,   566,    44,    43,     0,    76,    73,
      75,   563,    93,   562,     0,    88,   574,   561,    92,     0,
     572,     0,     0,    99,   574,   227,     0,   190,   191,     0,
     256,     0,     0,    50,    48,   574,    41,   105,     0,   551,
     549,     0,    57,     0,     0,   110,     0,   574,   574,   574,
     574,     0,     0,   133,   132,   134,   574,   136,   131,   135,
     140,     0,   148,   150,   574,   574,   574,     0,   503,   499,
     498,   126,     0,   123,   125,   121,   128,   574,   129,   494,
     476,   478,   480,   495,     0,   160,   574,   432,     0,     0,
     366,   365,     0,     0,     0,     0,     0,   287,     0,   337,
     292,   341,   327,     0,   317,   331,   330,   316,   312,     0,
       0,     0,     0,     0,   310,     0,     0,     0,   117,     0,
       0,   198,   218,     0,     0,     0,     0,   262,     0,     0,
     240,   241,   233,   242,   216,   196,   238,   234,   232,   235,
     236,   237,   239,   217,   213,   214,   200,   208,   207,   211,
     210,     0,     0,   201,   202,   206,   212,   203,   204,   205,
     215,     0,   275,   574,   505,   506,     0,   508,     0,     0,
       0,     0,    90,   574,     0,   119,   188,   255,     0,   543,
     101,     0,   100,     0,     0,     0,     0,   547,   574,     0,
      52,     0,   256,     0,   171,   172,   176,   175,   168,   173,
     177,   174,     0,   183,     0,     0,    81,     0,   574,   141,
       0,   412,   417,     0,   413,   574,   403,   506,   574,   153,
       0,     0,   574,   130,   574,   485,   484,   486,     0,   482,
       0,     0,   282,   435,   434,   433,     0,     0,   425,     0,
     465,   328,   314,   313,     0,     0,     0,   318,     0,     0,
     571,   567,     0,   193,   230,   229,   231,     0,   228,     0,
      40,   192,   379,   378,   381,     0,     0,     0,   377,   382,
       0,   507,    84,   572,    95,    89,   574,     0,     0,    97,
       0,    71,     0,   109,   552,   550,   556,   555,   554,     0,
      55,    56,     0,   574,     0,    59,    80,   122,     0,   143,
     142,   139,   574,   418,   574,     0,     0,     0,     0,     0,
       0,   516,   500,   504,     0,   479,   574,   574,     0,     0,
     437,   436,   574,   319,     0,     0,   323,   322,   199,     0,
       0,   574,     0,     0,   574,   209,     0,    96,     0,    91,
     574,    86,    72,   102,   548,   553,     0,   574,     0,   574,
       0,   416,     0,   415,   151,   574,     0,   513,     0,   515,
     517,     0,   509,   510,   124,     0,   472,   481,   473,     0,
     164,   163,   574,     0,     0,   321,   320,     0,   574,     0,
     574,     0,     0,     0,     0,     0,    94,    85,     0,   111,
       0,   167,     0,   144,   419,   420,   574,     0,   511,   512,
     514,     0,     0,   521,   522,     0,   574,     0,   161,   426,
     194,     0,   574,     0,   574,   574,   574,     0,   249,   574,
      87,   120,     0,     0,   414,   152,   518,     0,   471,   483,
     165,     0,   574,   220,     0,   574,     0,     0,     0,   574,
     219,     0,   137,     0,   519,   195,   221,     0,   243,   245,
       0,   226,   574,   574,   574,     0,     0,     0,   246,   248,
     403,     0,   224,   223,   222,   574,   138,   520,     0,   244,
     225,   247
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,     5,    99,   100,   101,   657,   780,   781,   782,
     783,   106,   395,   396,   784,   785,   699,   109,   110,   786,
     112,   787,   114,   788,   659,   202,   789,   117,   790,   665,
     532,   791,   369,   792,   379,   231,   390,   232,   793,   794,
     795,   796,   520,   125,   725,   574,   706,   126,   711,   860,
     951,  1000,    41,   566,   127,   128,   129,   130,   797,   881,
     734,  1021,   798,   799,   697,   848,   399,   400,   401,   554,
     800,   135,   540,   375,   801,   979,  1081,   902,   802,   803,
     804,   805,   806,   807,   808,   809,  1083,  1096,   810,   916,
     137,   811,   298,   183,   346,   184,   282,   283,   449,   284,
     575,   165,   384,   166,   319,   167,   168,   169,   244,    43,
      44,   285,   197,    46,    47,    48,    49,    50,   306,   307,
     348,   309,   310,   421,   862,   863,   952,  1044,   287,   313,
     289,   290,  1016,  1017,   427,   428,   579,   730,   731,   878,
     967,   879,    51,    52,   732,   577,   815,  1097,   869,   960,
    1009,  1010,   176,    53,   355,   393,    54,   179,    55,   259,
     691,   837,   291,   292,   668,   193,   356,   654,   185
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -895
static const yytype_int16 yypact[] =
{
     668,  4205,  4277,    49,    59,  3695,  -895,  -895,  -895,  -895,
    -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,
    -895,  -895,  -895,  -895,  -895,  -895,   151,  -895,  -895,  -895,
    -895,  -895,   148,   237,   259,    55,  -895,  -895,   196,   240,
     276,    96,   352,  4922,   781,  1447,   781,  -895,  -895,  -895,
    2090,  -895,    96,   276,  -895,   180,  -895,   359,   363,  4639,
    -895,   264,  -895,  -895,  -895,   372,  -895,  -895,    37,   407,
    4349,   425,  -895,  -895,   407,   438,   440,   460,   388,  -895,
    -895,   465,   344,    92,   239,   356,   158,   470,   216,   495,
     412,    77,  4710,  4710,   507,   512,   448,   519,   242,  -895,
    -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,   407,
    -895,  -895,  -895,  -895,  -895,  -895,  -895,  1542,  -895,  -895,
    -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,
    -895,  -895,  -895,  -895,  -895,  -895,  -895,    97,  4781,  -895,
     514,  -895,  -895,   517,   521,    96,    41,   612,  2135,  -895,
    -895,  -895,   781,  -895,  3334,   523,    18,  2263,  3031,    38,
    1338,  1720,   163,    96,  -895,  -895,     2,    28,     2,   258,
    1336,   464,  -895,  -895,  -895,  -895,  -895,   113,   319,  -895,
    -895,  -895,   541,  -895,   549,  -895,  -895,   737,  -895,  -895,
     612,    45,   737,   737,  -895,   546,  1702,  -895,   155,  1052,
      96,   113,   113,  -895,   737,  4567,  -895,  -895,  4639,  -895,
    -895,  -895,  -895,  -895,    96,   246,  -895,   206,   580,   113,
    -895,  -895,   737,   113,  -895,  -895,  -895,   587,  4639,   582,
     387,   552,   594,   737,   448,   587,  4639,  4639,    96,   448,
    2104,   321,   484,   737,  2217,   522,  -895,  -895,  1702,    96,
    1813,   508,  -895,   592,   593,   605,   113,  -895,  -895,   180,
     544,  -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,
    -895,  -895,  3031,   324,  3031,  3031,  3031,  3031,  3031,  3031,
    3031,  -895,   548,  -895,   616,   627,  1218,  2488,    24,  -895,
    -895,   587,   661,  -895,  -895,  3451,   341,   341,   629,   644,
    1017,   571,   640,  -895,  -895,  -895,   637,  3031,  -895,  -895,
    -895,  -895,  4353,  -895,  2488,   673,  3451,   653,    96,   297,
     258,  -895,   677,   297,   258,  -895,   597,  -895,  -895,  4639,
    2391,  -895,  4639,  2519,   681,  3213,  3216,   297,   258,   614,
    2345,  -895,  -895,   180,   695,  4639,  -895,  -895,  -895,  -895,
     698,   587,    96,  -895,  -895,   147,   707,  -895,  -895,   251,
       2,   409,  -895,   710,   711,   715,   706,   486,   717,   722,
    -895,   727,   728,  -895,  4852,  -895,    96,  -895,   736,   740,
    -895,   741,   743,  4710,  -895,  -895,  -895,  -895,  -895,  4710,
    -895,  -895,  -895,   744,  -895,  -895,   560,    51,   746,   689,
    -895,   756,  -895,    91,  -895,  -895,   165,  1354,  1354,  1354,
     164,   683,   760,   108,   765,   911,  1124,   693,  2345,   700,
     129,   748,   188,  -895,  3523,  1934,  -895,   766,  -895,   292,
    -895,  -895,  -895,  -895,   276,  -895,   612,  1967,  4852,   772,
    2980,  1507,  -895,  -895,  -895,  -895,  -895,  -895,  2135,  -895,
    -895,  -895,  3031,  3031,  3031,  3031,  3031,  3031,  3031,  3031,
    3031,  3031,  3031,  3031,  3031,  3031,  3031,  3031,  3031,  -895,
     306,   306,  1529,   709,   317,  -895,   428,  -895,  -895,   306,
     306,   520,   712,  1354,  1354,  3031,  2488,  -895,  4639,  3200,
      21,   774,  -895,  4639,  2647,   786,  -895,   795,  -895,  4714,
     796,  -895,  4801,   797,   800,   297,   258,   801,   297,   258,
    1786,   803,   810,  1126,   297,  -895,  -895,   549,   171,  -895,
    -895,   737,  2183,  -895,   832,   833,  -895,  -895,  -895,   414,
    1273,  2348,   841,  4639,  1702,   798,  -895,   387,  3797,   840,
    -895,   785,  4710,   361,   844,   838,   594,   561,   849,   737,
    4639,   104,   804,  4639,  -895,  -895,  -895,  1354,   393,  1449,
      76,  -895,  2473,  4992,   839,  4922,   421,  -895,   858,   683,
     863,   139,   816,   822,   569,  -895,   554,  -895,     2,   835,
    -895,  -895,   871,  -895,    96,  3031,  2775,  2903,  3159,    14,
    1447,   876,   616,  1829,  1829,  1822,  1822,  2360,  1420,  2980,
    2622,  3300,  1507,  1076,  1076,   705,   705,  -895,  -895,  -895,
     712,  -895,  -895,  -895,  -895,   306,   547,    28,  4926,   878,
     578,   712,  -895,  1449,  1449,   886,  -895,  4938,  1449,  -895,
    -895,  -895,  -895,  1449,   882,   883,   884,   891,  1215,   297,
     258,   894,   897,   910,   297,  -895,  -895,  -895,   587,  3899,
    -895,   918,  -895,    51,   919,  -895,  -895,  1968,  -895,  -895,
     587,  -895,  -895,  -895,   923,  -895,  1651,   587,  -895,   917,
     199,   568,  1273,  -895,  1651,  -895,   921,  -895,  -895,  4001,
      73,  4852,   445,  -895,  -895,  4639,  -895,  -895,   834,  -895,
     208,   870,  -895,   936,   933,  -895,    96,  3263,   756,  -895,
    1651,   229,  1449,  -895,  -895,  -895,  1934,  -895,  -895,  -895,
    -895,   265,  -895,  -895,   920,   767,  4639,  3031,  -895,  -895,
    -895,  -895,  1702,  -895,  -895,  -895,  -895,     2,  -895,  -895,
     939,  -895,   805,  -895,  1968,  -895,     2,  2488,  3031,  3031,
    3159,  3593,  3031,   943,   945,   946,   948,  -895,  3031,  -895,
    -895,  -895,  -895,   632,   297,  -895,  -895,   297,   297,  1449,
    1449,   941,   947,   954,   297,  1449,   956,   958,  -895,   737,
     737,  -895,  -895,   961,   915,   935,   937,   855,   969,   113,
    -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,
    -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,  -895,
    -895,   965,  1968,  -895,  -895,  -895,  -895,  -895,  -895,  -895,
    -895,  4420,   968,  4639,   725,  -895,   104,  -895,  2183,  1044,
     737,   953,  -895,  1651,   975,  -895,  -895,   587,  1702,   119,
    -895,  4710,  -895,   978,   136,   113,   213,  -895,  2135,   217,
    -895,   967,    37,   390,  -895,  -895,  -895,  -895,  -895,  -895,
    -895,  -895,  4492,  -895,  4103,   981,  -895,   569,  4639,  -895,
     458,  -895,   113,   329,  -895,  4639,   409,   973,   952,  -895,
     988,  2724,  1934,  -895,   835,  -895,  -895,  -895,    96,  -895,
     986,  1968,  -895,  2488,  2488,  2488,  3031,  3031,  -895,  4852,
    2613,  -895,   297,   297,  1449,   983,   987,   297,  1449,  1449,
    -895,  -895,  1968,  -895,  -895,  -895,  -895,   113,  -895,   994,
    -895,  -895,   962,   963,   964,  4852,   974,  3198,   976,   247,
    1002,  -895,  -895,   587,  1009,  -895,  1651,  1253,   104,  -895,
    1010,  -895,  1016,  -895,  -895,   208,  -895,  -895,   208,   960,
    -895,  -895,  4852,  4639,  1702,  -895,  -895,  -895,  1019,  -895,
    -895,  -895,   920,  1018,   920,  1619,  1030,  1032,   409,    96,
     461,  -895,  -895,  -895,   569,  -895,  1025,   835,  1646,  1029,
    2488,  2488,  1447,   297,  1449,  1449,   297,   297,  -895,  1968,
    1037,  4639,     9,  3031,  3523,  -895,  1036,  -895,  1040,  -895,
    1651,  -895,  -895,  -895,  -895,  -895,  1043,   387,   985,  1651,
    1048,  -895,  3031,   113,  -895,  1934,   487,  -895,  1051,  1038,
    1047,   351,  -895,  -895,  -895,  1053,  -895,  -895,  -895,    96,
    -895,  -895,  1934,  1646,  1059,   297,   297,  1061,  4639,  1068,
    4639,  1070,  1073,    36,  2852,  1075,  -895,  -895,  1074,  -895,
    1077,  -895,     1,  -895,  -895,  2488,   920,   569,  -895,  -895,
    -895,    96,  1071,  -895,  -895,  1080,  1025,   569,  -895,  -895,
    -895,  1082,  1651,  1089,  4639,  4639,  4639,  1078,  -895,   767,
    -895,  -895,  4852,   458,  -895,  -895,  1083,  1087,  -895,  -895,
    -895,  1968,  1651,  -895,   340,  1651,  1096,  1097,  1098,  4639,
    -895,  1094,  -895,  1100,  -895,  -895,  -895,   449,  -895,  -895,
     409,  -895,  1651,  1651,  1651,  1103,   458,  1102,  -895,  -895,
     409,  1109,  -895,  -895,  -895,  1651,  -895,  -895,  1110,  -895,
    -895,  -895
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -895,  -895,  -344,  -895,  -895,  -895,  -895,    43,    47,    -4,
      56,  -895,   623,  -895,    58,    62,  -895,  -895,  -895,    67,
    -895,    68,  -895,    75,  -895,  -895,    79,  -895,    82,  -523,
    -642,    83,  -895,    93,  -895,  -351,   608,   -79,    95,   101,
     115,   116,  -895,   459,  -811,  -666,  -895,  -895,  -895,  -849,
    -772,  -895,  -130,  -895,  -895,  -895,  -895,  -895,     7,  -895,
    -895,   137,    11,    12,  -895,  -895,   218,  -895,   609,   469,
     121,  -895,  -895,  -895,  -708,  -895,  -895,  -895,  -895,   471,
    -895,   472,   124,   478,  -895,  -895,  -895,  -195,  -895,  -895,
    -895,    -2,    60,  -895,   662,    22,   346,  -895,   586,   739,
     -34,  -572,  -531,   -40,  1162,  -133,  -144,   -57,    26,   -37,
    -895,   -56,    33,   -22,   631,  -524,  1140,  -895,  -357,  -895,
    -154,  -895,  -895,  -895,  -894,  -895,   193,  -895,  1142,  -118,
    -489,  -895,  -895,   141,   769,  -895,  -895,  -895,   330,  -895,
    -895,  -895,  -222,   -33,   238,   649,  -398,  -573,   156,  -895,
    -895,   175,   -15,   984,   -97,  -895,   951,  -189,  -124,  1050,
    -895,  -381,   701,  -895,   553,   183,  -202,  -512,     0
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -575
static const yytype_int16 yytable[] =
{
       6,   104,   308,   138,   525,   139,   676,   153,   249,   173,
     299,   164,   131,   229,   196,   403,   132,   133,   745,   671,
     380,   152,   258,    42,    57,   149,   880,   578,   695,   821,
     288,   538,   545,   652,    45,    45,   230,   230,   412,   708,
     857,   315,   652,   188,     8,   172,   947,   859,   102,  1030,
      58,   353,   103,   341,   258,   742,   156,   353,  1001,    60,
    1003,   105,   622,   107,   303,   238,  -290,   108,   329,   194,
     435,   170,   111,   113,   194,   203,  1066,   243,   158,   188,
     115,   226,   250,   188,   116,   330,   386,   118,   119,    59,
     350,   354,    45,   816,   911,   145,   357,   550,   120,     8,
     121,   824,   363,   320,   324,   251,   122,   367,   661,   194,
     188,     8,  1072,   338,   260,   294,     8,   622,   246,   182,
     123,   124,   189,   304,   305,   326,   134,   855,  -290,   136,
      36,    37,   328,  -181,   331,  1031,   392,   623,  1032,     8,
     743,   148,   868,   744,   149,   162,   414,   662,   293,   822,
     663,   299,  1074,  1014,   515,   190,   360,   352,   189,   256,
     257,     8,   931,   474,   476,  1067,   172,   481,   172,   210,
     281,   353,   299,   969,   211,   646,   649,   188,   727,   717,
     300,   286,   247,   564,   347,    36,    37,    45,   325,   521,
     385,   682,   702,  1073,   978,  -181,   679,    36,    37,   140,
     141,   522,    36,    37,   216,   353,   964,   308,   419,   361,
     425,    38,    27,    28,    29,    40,     8,   647,   664,   142,
     220,   940,   148,    38,   556,    36,    37,    40,   572,   573,
     387,  -410,   391,   394,  -410,   353,  1075,   404,    45,   320,
     324,    45,  1106,   338,   162,   418,  1080,    36,    37,   423,
     928,   430,   164,  -529,     8,   934,   170,   200,   148,   402,
     376,    45,   221,   941,  -410,   370,   409,   148,   371,    45,
      45,  1027,   856,    38,   474,   476,   481,    40,   506,   509,
    -543,  -543,   987,   212,   450,   143,   172,   918,   382,   373,
     985,   148,   239,   922,    30,    31,   398,   177,   332,   162,
     374,  1092,    36,    37,  -543,   858,   924,   144,   578,   148,
     546,   859,   170,    33,    34,   333,   213,   867,   537,   650,
     178,   472,     8,  -254,     8,   835,   524,   230,    45,   652,
     288,   403,   937,   230,  1116,   581,   146,   493,  -574,  1047,
      36,    37,   489,   147,     8,   237,   295,   693,   836,    45,
     616,   617,   496,   988,   494,   854,  1057,   329,   612,     8,
     172,   413,    45,   157,   438,    45,    38,   517,   426,   317,
      40,   954,   439,  1095,   330,     6,   955,   407,    45,   148,
     408,   154,   590,  1098,   186,   409,  1099,   162,   576,   497,
       8,  1053,   500,   386,  1100,   151,     8,  1054,   157,   188,
    1024,   555,   180,   584,   683,   991,   181,   684,    36,    37,
      36,    37,   187,   320,   324,   338,   921,  1038,   658,   583,
     188,   567,   506,   509,   616,   172,  1042,   154,   209,   430,
      36,    37,   338,   359,   669,   155,    38,   256,   342,   559,
      40,   155,    30,    31,   157,    36,    37,   192,   214,   661,
     409,   188,   223,   640,   188,   224,    38,    45,   225,   318,
     159,    33,    34,   589,   713,   199,  1040,   714,   329,   613,
     281,    38,   206,   207,   578,    40,    36,    37,   201,   163,
     204,   286,    36,    37,   568,   330,   230,     8,   830,  1084,
     666,   663,  1108,   943,   674,  1109,   868,   385,   949,   831,
     205,   950,    38,  1110,  1012,   208,   159,  1013,    38,   957,
     219,     8,   159,   160,   304,   305,   161,   347,   700,   241,
       6,    45,   242,   162,   413,   163,    45,   530,   531,   162,
    1048,   163,   322,  1049,   104,   222,   138,   387,   139,   681,
       6,   139,   726,   712,   728,   131,   391,   233,   619,   132,
     133,   741,   234,   625,   994,   673,   164,   995,   640,   236,
     329,   614,   252,   170,     8,   253,    45,   900,   901,   254,
     721,   302,   694,    36,    37,   402,   172,   330,   172,   733,
     340,   102,   344,    45,   426,   103,    45,   329,   749,   358,
     172,   345,   450,   353,   105,   388,   107,    36,    37,    38,
     108,   548,   549,    40,   330,   111,   113,   578,   156,   819,
     820,   722,   723,   115,   325,   724,   170,   116,   493,   752,
     118,   119,   318,    38,   578,   828,   377,    40,   383,   814,
     158,   120,   823,   121,   570,   494,   389,   814,   418,   122,
     431,   432,   571,   572,   573,   104,   433,   138,   436,   139,
      36,    37,   447,   123,   124,   909,   131,   812,   448,   134,
     132,   133,   136,   814,    30,    31,   817,   451,   469,   576,
     477,   867,   493,   891,   817,   104,   688,   138,   814,   139,
     689,   249,   872,    33,    34,   478,   131,   483,   484,   494,
     132,   133,   102,   873,   485,   852,   103,   139,   555,     6,
     817,   491,   882,   741,   845,   105,   172,   107,   846,   847,
     861,   108,   308,   488,   864,   817,   111,   113,    45,   152,
     288,   403,   102,   669,   115,   495,   103,   172,   116,   503,
     510,   118,   119,   162,   812,   105,   172,   107,   516,   518,
       8,   108,   120,   188,   121,   833,   111,   113,   523,    45,
     122,   526,   932,  1118,   115,   917,   528,   527,   116,   533,
     529,   118,   119,   534,   123,   124,   325,   496,   535,   191,
     134,   536,   120,   136,   121,   230,   870,   541,   251,   926,
     122,   542,   543,   980,   544,   547,   814,   551,   930,     1,
       2,     3,   227,   552,   123,   124,   917,   235,   553,   557,
     134,   558,   812,   136,  1008,   570,    66,   865,   561,   562,
     238,   563,   580,   571,   572,   573,    27,    28,    29,   251,
     591,   866,   624,   817,   565,   611,    36,    37,   615,   680,
      27,    28,    29,   972,   628,   576,   629,   631,   293,   466,
     467,   468,   304,   305,   675,   633,    45,   570,   634,   635,
     104,   641,   138,   386,   139,   571,   572,   573,   642,   982,
     281,   131,    78,    79,    80,   132,   133,    82,   961,    83,
      84,   286,   172,   920,   733,   655,   656,   425,   875,   876,
     877,   812,   672,   678,   685,   686,   997,   990,   351,   814,
    1101,    45,   692,   351,   351,   696,   710,   102,    45,   715,
     351,   103,   812,   716,   419,   351,   719,  1112,  1113,  1114,
     105,   720,   107,   729,     8,  1005,   108,   735,   948,   751,
    1120,   111,   113,   351,   746,   956,   817,   755,  1022,   115,
     759,   760,   761,   116,   351,   397,   118,   119,   164,   762,
     406,   351,   765,   404,   351,   766,  1111,   120,   861,   121,
     861,   413,   864,   814,   864,   122,   308,   385,   767,   504,
     769,   770,   814,   813,   825,   402,  1018,   733,   576,   123,
     124,   818,   172,   838,   834,   134,    45,   839,   136,   812,
     840,   874,    70,  1022,   886,   576,   887,   888,   889,   894,
     817,   904,   150,   907,   927,   895,   171,   387,   170,   817,
      36,    37,   896,   175,   898,   172,   899,   903,  1033,   908,
     409,   905,   910,   906,    45,  -197,  1091,    45,   929,   933,
       8,   942,   172,   170,   946,   814,    38,   958,   959,   962,
      40,   974,   814,   968,   981,   975,   215,   218,  -252,  -251,
    -253,  1029,   861,   986,  1035,   814,   864,     8,   814,   318,
     983,   820,  -250,   992,   198,     8,  1018,   154,   188,   993,
     999,    45,   817,    45,   996,   814,   814,   814,   245,   817,
     217,  1006,  1002,  1015,   157,  1007,  1023,  1028,   814,  1036,
    1051,   812,   817,  1037,   359,   817,  1039,   925,  1061,  1041,
    1063,  1043,   155,   362,  1050,  1052,   255,    45,    45,    45,
    1059,  1055,   817,   817,   817,   301,    36,    37,  1060,  1062,
    1064,   321,   321,  1065,   327,   817,  1069,  1070,  1089,  1077,
    1071,   339,    45,  1082,  1086,  1087,  1088,     8,  1078,     8,
    1085,  1093,    38,    36,    37,  1094,   159,  1102,  1103,  1104,
     859,    36,    37,   479,  1115,   651,   480,   245,  1107,  1105,
    1117,   364,  1119,  1121,   687,   163,   844,   304,   305,    38,
    1058,   998,   698,   159,   413,   372,   413,   853,   849,   850,
     241,   349,   507,   242,   643,   851,   349,   349,   747,   645,
     162,   171,   163,   349,   939,   365,   366,   592,   349,   405,
     174,   411,   321,   321,   707,   417,  1046,  1079,   582,   420,
     150,   245,   429,   378,   965,  1019,   349,   381,   464,   465,
     466,   467,   468,    36,    37,    36,    37,   349,     8,   648,
     718,     8,   351,   653,   410,  1090,  1076,   349,   343,     0,
     660,   667,   670,   422,     0,   832,     0,   171,     0,    38,
     434,    38,     0,    40,     0,    40,   473,   475,   475,     0,
     351,   482,   667,     0,     0,   413,     8,     0,   154,   701,
    -574,     0,   318,   763,   318,     0,   155,   490,     0,   492,
       0,     0,   156,     0,     0,   157,     8,   661,     0,   188,
       0,     0,     0,     0,     0,     0,   321,   321,     0,     0,
       0,   321,     0,   359,   158,     0,   989,     0,     0,   312,
     314,   155,     0,   519,    36,    37,     0,    36,    37,     0,
     411,     0,     0,   359,     0,     0,   662,     0,     0,   663,
       0,   155,  -574,   323,     0,     0,     0,   539,     0,     0,
      38,     0,   337,    38,    40,     0,     0,   159,     0,     8,
       0,     8,    36,    37,   160,     0,     0,   161,     0,     0,
       0,     0,     0,   318,   162,     0,   163,     8,   475,   475,
     475,     0,    36,    37,   560,     0,   321,   321,    38,   321,
       0,     0,   159,   667,     0,   569,   316,     0,   316,   241,
       0,   827,   242,   667,   334,     0,   317,   664,    38,   162,
       0,   163,   159,   157,   359,   157,     0,     0,     0,   241,
       0,     0,   242,     0,   323,     0,   337,     0,     0,   162,
       0,   163,     0,     0,   437,     0,   440,   441,   442,   443,
     444,   445,   446,   610,     0,    36,    37,    36,    37,     0,
       0,     0,     0,     0,   475,   475,     0,     0,     0,     0,
     621,     0,     0,    36,    37,     0,     0,     0,     0,   486,
       8,    38,     8,    38,     0,    40,     0,    40,     0,     0,
       0,   321,   335,     0,   321,   336,     0,     0,     0,    38,
     351,   351,   499,   159,   318,   502,   318,     0,     0,     0,
       0,   245,     0,     0,     0,   245,     0,   154,   171,   413,
       0,     0,   163,     0,     0,   155,     0,   505,   508,     0,
       0,   156,   514,     0,   157,   349,     0,     0,   475,   245,
     321,     0,     0,   321,   709,   349,     0,   667,     0,   923,
       0,   351,     0,   158,     0,     0,     0,   452,   453,   454,
     455,   690,     8,   349,     0,   736,    36,    37,    36,    37,
       0,   171,     0,   827,     0,     8,   458,   459,   460,   461,
     462,   463,   464,   465,   466,   467,   468,     0,     0,     0,
       0,     0,    38,     0,    38,     0,   159,     0,    40,   295,
       0,     0,     0,   160,   321,   321,   161,   505,   508,   321,
     514,     0,   240,   162,   321,   163,   157,   318,     0,   321,
     155,     0,     0,     0,   593,   594,   595,   596,   597,   598,
     599,   600,   601,   602,   603,   604,   605,   606,   607,   608,
     609,     0,     0,     0,   452,   453,   454,   455,    36,    37,
       0,     0,     8,   245,     0,     0,     0,   618,     0,   667,
       0,    36,    37,     0,     0,     0,   627,   462,   463,   464,
     465,   466,   467,   468,    38,     0,     0,   841,    40,     8,
       0,   620,     0,   321,     0,   479,     0,    38,   480,   359,
       0,   159,  1004,     0,     0,   829,     0,   155,   241,     0,
       0,   242,   639,   245,     0,   644,     0,     0,   162,     0,
     163,     0,     0,     0,     0,     0,   359,     0,     0,  1020,
       0,     0,     0,     0,   155,     0,     0,     0,     0,     0,
      27,    28,    29,     0,     0,     8,     0,     0,    36,    37,
     321,   321,     0,     0,     0,     0,   321,     0,     0,     0,
       0,   620,     0,     8,   639,     0,     0,   737,   599,   602,
     609,   570,     0,     0,    38,    36,    37,     0,   159,   571,
     572,   573,   359,     0,     0,   241,     0,     0,   242,     0,
     155,     0,     0,   349,   349,   162,     0,   163,     0,     0,
     316,    38,   919,     0,     0,   159,     0,     0,   322,     0,
     245,     0,   241,     0,     0,   242,     0,   157,     0,   245,
       0,     0,   162,     0,   163,   753,   754,     0,     0,     8,
     757,    36,    37,     0,     0,   758,     0,     0,     0,     0,
     764,     0,     0,     0,   349,     0,     0,     0,     0,    36,
      37,     0,     0,     0,     0,     0,     8,    38,   935,   936,
     938,   159,     0,     0,     0,     0,   316,     0,   241,   966,
       0,   242,     0,     0,   636,    38,     0,     0,   162,    40,
     163,     0,     0,   157,     0,   321,   953,     0,     0,   321,
     321,     0,     0,   424,     0,     0,     0,     0,   318,   871,
       0,   155,     0,     0,   753,     0,     0,     0,   245,     0,
       0,     0,     0,     0,     0,    36,    37,     0,   245,     0,
     883,   884,   444,     0,   885,     0,     0,     0,     0,     0,
     890,     0,     0,     0,     0,   245,     0,     0,     0,     0,
       0,    38,    36,    37,     0,    40,   245,     0,     0,     0,
    1011,     0,   637,     0,     0,   638,     0,     0,     0,   245,
       0,   892,   893,   171,   318,   321,   321,   897,    38,   452,
     453,     0,   159,     0,     0,   569,     0,     0,     0,   241,
       0,     0,   242,     0,     0,     0,     0,     0,   171,   162,
       0,   163,   462,   463,   464,   465,   466,   467,   468,   462,
     463,   464,   465,   466,   467,   468,     0,     0,     0,   771,
    1056,  -574,    62,     0,   245,     0,    63,    64,    65,     0,
       0,     0,     0,    27,    28,    29,     0,   953,   156,    66,
    -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,
    -574,  -574,  1011,  -574,  -574,  -574,  -574,  -574,   585,     0,
     158,   772,    68,     0,   570,  -574,     0,  -574,  -574,  -574,
    -574,  -574,   571,   572,   573,     0,     0,     0,   970,   971,
      70,    71,    72,    73,   773,    75,    76,    77,  -574,  -574,
    -574,   774,   775,   776,     0,    78,   777,    80,     0,    81,
      82,   778,    83,    84,  -574,  -574,   973,  -574,  -574,    85,
     976,   977,     0,    89,     0,    91,    92,    93,    94,    95,
      96,     0,     0,     0,   452,   453,   454,   455,     0,   456,
       0,    97,     0,  -574,     0,     0,    98,  -574,  -574,     0,
       0,     0,   457,   586,   459,   460,   587,   462,   463,   464,
     465,   466,   467,   588,     0,     0,   779,     8,     0,     0,
     188,     0,     9,    10,    11,    12,    13,    14,    15,    16,
       0,    18,     0,    20,     0,  1034,    22,    23,    24,    25,
       0,     0,     0,     0,     0,     0,  1025,  1026,     8,     0,
       0,   188,   261,     0,  1045,     0,   262,   263,   264,   265,
     266,   267,   268,   269,   270,   271,     0,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,     0,
      21,    22,    23,    24,    25,   272,     0,     0,     0,     0,
       0,     0,     0,    26,    27,    28,    29,    30,    31,   188,
     273,     0,     0,    36,    37,     0,   264,   265,   266,   267,
     268,   269,   270,   271,     0,    32,    33,    34,     0,   304,
     305,     0,     0,     0,     0,     0,     0,     0,     0,    38,
       8,    35,     0,    40,    36,    37,     0,     0,     0,     0,
     407,     0,     0,   408,     0,     0,     0,     0,     0,     0,
     162,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      38,     0,     0,    39,    40,     0,     0,   413,     0,     0,
       0,   274,     0,     0,   275,   334,     8,   276,   277,   188,
     261,   278,   279,   280,   262,   263,   264,   265,   266,   267,
     268,   269,   270,   271,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,     0,    21,    22,
      23,    24,    25,   272,     0,     0,    36,    37,     0,     0,
       0,     0,    27,    28,    29,    30,    31,     0,   273,     0,
       0,   311,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    38,    32,    33,    34,    40,     0,     0,     0,
       0,     0,     0,   415,     0,     0,   416,     0,     8,    35,
       0,     8,    36,    37,   188,   318,     0,     0,     0,     0,
       0,   264,   265,   266,   267,   268,   269,   270,   271,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    38,     0,
       0,     0,    40,     0,     0,   413,     0,     0,     0,   274,
       0,     0,   275,   511,     8,   276,   277,   188,   261,   278,
     279,   280,   262,   263,   264,   265,   266,   267,   268,   269,
     270,   271,     0,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,     0,    21,    22,    23,    24,
      25,   272,     0,     0,    36,    37,   748,    36,    37,     0,
      27,    28,    29,    30,    31,     0,   273,     0,     0,   498,
       0,     0,     0,   304,   305,     0,     0,     0,     0,     0,
      38,    32,    33,    34,    40,     0,     0,   452,   453,   454,
     455,   512,   456,     0,   513,     0,     8,    35,     0,     0,
      36,    37,     0,   318,     0,   457,   458,   459,   460,   461,
     462,   463,   464,   465,   466,   467,   468,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    38,     0,     0,     0,
      40,     0,     0,   413,     0,     0,     0,   274,     0,     0,
     275,   636,     8,   276,   277,   188,   261,   278,   279,   280,
     262,   263,   264,   265,   266,   267,   268,   269,   270,   271,
       0,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,     0,    21,    22,    23,    24,    25,   272,
       0,     0,    36,    37,     0,     0,     0,     0,    27,    28,
      29,    30,    31,     0,   273,     0,     0,   501,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    38,    32,
      33,    34,    40,     0,     0,   452,   453,   454,   455,   637,
     456,     0,   638,     0,     0,    35,     0,     0,    36,    37,
       0,   318,     0,   457,   458,   459,   460,   461,   462,   463,
     464,   465,   466,   467,   468,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    38,     0,     0,     0,    40,     0,
       0,     0,     0,     0,     0,   274,     0,     0,   275,     0,
       8,   276,   277,   188,   261,   278,   279,   280,   262,   263,
     264,   265,   266,   267,   268,   269,   270,   271,     0,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,     0,    21,    22,    23,    24,    25,   272,     0,     0,
       0,     0,     0,     0,     0,     0,    27,    28,    29,    30,
      31,     0,   273,     0,     0,   626,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    32,    33,    34,
     452,   453,   454,   455,     0,     0,     0,     0,     0,   452,
     453,   454,   455,    35,     0,     0,    36,    37,   457,   458,
     459,   460,   461,   462,   463,   464,   465,   466,   467,   468,
     460,   461,   462,   463,   464,   465,   466,   467,   468,     0,
       0,     0,    38,     0,     0,   963,    40,     0,     0,     0,
       0,     0,     0,   274,     0,     0,   275,     0,     8,   276,
     277,   188,   261,   278,   279,   280,   262,   263,   264,   265,
     266,   267,   268,   269,   270,   271,     0,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,     0,
      21,    22,    23,    24,    25,   272,   738,     0,     0,     0,
       0,     0,     0,     0,    27,    28,    29,    30,    31,     0,
     273,   452,   453,   454,   455,     0,   456,     0,     0,     0,
       0,     0,     0,     0,     0,    32,    33,    34,     0,   457,
     458,   459,   460,   461,   462,   463,   464,   465,   466,   467,
     468,    35,     0,     0,    36,    37,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      38,     0,     0,     0,    40,  1068,     0,     0,     0,     0,
       0,   274,     0,     0,   275,     0,     8,   276,   277,   188,
     261,   278,   279,   280,   262,   263,   264,   265,   266,   267,
     268,   269,   270,   271,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,     0,    21,    22,
      23,    24,    25,   272,   739,     0,     0,     0,     0,     0,
       0,     0,    27,    28,    29,    30,    31,     0,   273,   452,
     453,   454,   455,     0,   456,     0,     0,     0,     0,     0,
       0,     0,     0,    32,    33,    34,     0,   457,   458,   459,
     460,   461,   462,   463,   464,   465,   466,   467,   468,    35,
       0,     0,    36,    37,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    38,     0,
       0,     0,    40,     0,     0,     0,     0,     0,     0,   274,
       0,     0,   275,     0,     8,   276,   277,   188,   261,   278,
     279,   280,   262,   263,   264,   265,   266,   267,   268,   269,
     270,   271,     0,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,     0,    21,    22,    23,    24,
      25,   272,     0,     0,     0,     0,     0,     0,     0,     0,
      27,    28,    29,    30,    31,     0,   273,   452,   453,   454,
     455,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    32,    33,    34,     0,     0,     0,   459,   460,   461,
     462,   463,   464,   465,   466,   467,   468,    35,     0,     0,
      36,    37,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    38,     0,     0,     0,
      40,     0,     0,     0,     0,     0,     0,   274,     0,     0,
     275,     0,     8,   276,   277,   188,   261,   278,   279,   280,
     262,   263,   264,   265,   266,   267,   268,   269,   270,   271,
       0,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,     0,    21,    22,    23,    24,    25,   272,
       0,     8,     0,     8,     0,     0,     0,     0,    27,    28,
      29,    30,    31,     0,   273,     0,     8,     0,     0,     8,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    32,
      33,    34,     0,     0,     0,     0,     0,     0,   984,     0,
     316,     0,     0,     0,     0,    35,   155,     0,    36,    37,
       0,     0,     0,   316,     0,     0,   316,   157,     0,     0,
       0,   504,     0,     0,   507,     0,     0,     0,     0,     0,
     157,     0,     0,   157,    38,     0,     0,     0,    40,     0,
       0,     0,     0,     0,    66,     0,     0,    36,    37,    36,
      37,   276,   277,     0,     0,   740,   279,   280,     0,     0,
       0,     0,    36,    37,     0,    36,    37,   842,     0,     0,
       0,     0,     0,    38,     0,    38,     0,   159,     0,    40,
       0,     0,     0,     0,   241,     0,   479,   242,    38,   480,
       0,    38,    40,     0,   162,    40,   163,     8,   318,     0,
      78,    79,    80,     0,     0,    82,   778,    83,    84,     0,
       0,   318,     0,     0,   318,     0,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,     0,    21,
      22,    23,    24,    25,   295,     0,     0,     0,     0,     0,
       0,   843,    26,    27,    28,    29,    30,    31,     0,     0,
       0,   157,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    32,    33,    34,   452,   453,   454,
     455,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      35,     0,     0,    36,    37,     0,     0,     0,     0,   461,
     462,   463,   464,   465,   466,   467,   468,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    38,
       0,     0,    39,    40,     8,     0,     0,     0,     0,     0,
     296,     0,     0,   297,     0,     0,     0,     0,     0,     0,
     162,     0,     0,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,     0,    21,    22,    23,    24,
      25,   295,     0,     0,     0,     0,     0,     0,     0,    26,
      27,    28,    29,    30,    31,     0,     0,     0,   157,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    32,    33,    34,     0,     0,     8,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    35,     0,     0,
      36,    37,     0,     0,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,     0,    21,    22,
      23,    24,    25,     0,     0,     0,    38,     0,     0,    39,
      40,    26,    27,    28,    29,    30,    31,   470,     0,     0,
     471,     0,     0,     0,     0,     0,     0,   162,     0,     0,
       0,     0,     0,    32,    33,    34,     8,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    35,
       0,     0,    36,    37,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,     0,    21,    22,
      23,    24,    25,     0,     0,     0,     0,     0,    38,     0,
       0,    39,    40,     0,     0,    30,    31,     0,     0,   407,
       0,     0,   408,     0,     0,     0,     0,     0,     0,   162,
       0,     0,     0,    32,    33,    34,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    35,
       0,     0,    36,    37,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,    61,     0,  -574,    62,
       0,     0,     0,    63,    64,    65,     0,     0,    38,     0,
       0,     0,    40,     0,     0,     0,    66,  -574,  -574,  -574,
    -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,   162,
    -574,  -574,  -574,  -574,  -574,     0,     0,     0,    67,    68,
       0,     0,     0,     0,  -574,  -574,  -574,  -574,  -574,     0,
       0,    69,     0,     0,     0,     0,     0,    70,    71,    72,
      73,    74,    75,    76,    77,  -574,  -574,  -574,     0,     0,
       0,     0,    78,    79,    80,     0,    81,    82,     0,    83,
      84,  -574,  -574,     0,  -574,  -574,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    61,     0,
    -574,    62,     0,     0,     0,    63,    64,    65,    97,     0,
    -574,     0,     0,    98,  -574,     0,     0,     0,    66,  -574,
    -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,
    -574,     0,  -574,  -574,  -574,  -574,  -574,     0,     0,     0,
      67,    68,     0,     0,   677,     0,  -574,  -574,  -574,  -574,
    -574,     0,     0,    69,     0,     0,     0,     0,     0,    70,
      71,    72,    73,    74,    75,    76,    77,  -574,  -574,  -574,
       0,     0,     0,     0,    78,    79,    80,     0,    81,    82,
       0,    83,    84,  -574,  -574,     0,  -574,  -574,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      61,     0,  -574,    62,     0,     0,     0,    63,    64,    65,
      97,     0,  -574,     0,     0,    98,  -574,     0,     0,     0,
      66,  -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,
    -574,  -574,  -574,     0,  -574,  -574,  -574,  -574,  -574,     0,
       0,     0,    67,    68,     0,     0,   768,     0,  -574,  -574,
    -574,  -574,  -574,     0,     0,    69,     0,     0,     0,     0,
       0,    70,    71,    72,    73,    74,    75,    76,    77,  -574,
    -574,  -574,     0,     0,     0,     0,    78,    79,    80,     0,
      81,    82,     0,    83,    84,  -574,  -574,     0,  -574,  -574,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    61,     0,  -574,    62,     0,     0,     0,    63,
      64,    65,    97,     0,  -574,     0,     0,    98,  -574,     0,
       0,     0,    66,  -574,  -574,  -574,  -574,  -574,  -574,  -574,
    -574,  -574,  -574,  -574,  -574,     0,  -574,  -574,  -574,  -574,
    -574,     0,     0,     0,    67,    68,     0,     0,   826,     0,
    -574,  -574,  -574,  -574,  -574,     0,     0,    69,     0,     0,
       0,     0,     0,    70,    71,    72,    73,    74,    75,    76,
      77,  -574,  -574,  -574,     0,     0,     0,     0,    78,    79,
      80,     0,    81,    82,     0,    83,    84,  -574,  -574,     0,
    -574,  -574,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    61,     0,  -574,    62,     0,     0,
       0,    63,    64,    65,    97,     0,  -574,     0,     0,    98,
    -574,     0,     0,     0,    66,  -574,  -574,  -574,  -574,  -574,
    -574,  -574,  -574,  -574,  -574,  -574,  -574,     0,  -574,  -574,
    -574,  -574,  -574,     0,     0,     0,    67,    68,     0,     0,
       0,     0,  -574,  -574,  -574,  -574,  -574,     0,     0,    69,
       0,     0,     0,   945,     0,    70,    71,    72,    73,    74,
      75,    76,    77,  -574,  -574,  -574,     0,     0,     0,     0,
      78,    79,    80,     0,    81,    82,     0,    83,    84,  -574,
    -574,     0,  -574,  -574,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,     7,     0,     8,     0,
       0,     0,     0,     0,     0,     0,    97,     0,  -574,     0,
       0,    98,  -574,     0,     0,     0,     0,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,     0,
      21,    22,    23,    24,    25,     0,     0,     0,     0,     0,
       0,     0,     0,    26,    27,    28,    29,    30,    31,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    32,    33,    34,    56,     0,
       8,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    35,     0,     0,    36,    37,     0,     0,     0,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,     0,    21,    22,    23,    24,    25,     0,     0,     0,
      38,     0,     0,    39,    40,    26,    27,    28,    29,    30,
      31,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    32,    33,    34,
     195,     0,     8,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    35,     0,     0,    36,    37,     0,     0,
       0,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,     0,    21,    22,    23,    24,    25,     0,
       0,     0,    38,     0,     0,    39,    40,     0,    27,    28,
      29,    30,    31,     0,     0,     0,     0,     0,     0,     0,
       0,   487,     0,     0,     0,     0,     0,     0,     0,    32,
      33,    34,     0,     8,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    35,     0,     0,    36,    37,
       0,     0,     9,    10,    11,    12,    13,    14,    15,    16,
     912,    18,   913,    20,     0,   914,    22,    23,    24,    25,
     452,   453,   454,   455,    38,   456,     0,     0,    40,    27,
      28,    29,    30,    31,     0,     0,     0,     0,   457,   458,
     459,   460,   461,   462,   463,   464,   465,   466,   467,   468,
      32,    33,    34,     0,     0,     8,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    35,   248,     0,    36,
      37,     0,     0,     0,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,     0,    21,    22,    23,
      24,    25,     0,     0,     0,    38,     0,     0,     0,    40,
     915,    27,    28,    29,    30,    31,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    32,    33,    34,     0,     0,     0,     0,     0,
       8,     0,     0,     0,     0,     0,     0,     0,    35,   944,
     368,    36,    37,     0,     0,     0,     0,     0,     0,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,     0,    21,    22,    23,    24,    25,    38,     0,     0,
       0,    40,   915,     0,     0,    26,    27,    28,    29,    30,
      31,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    32,    33,    34,
       0,     0,     8,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    35,     0,     0,    36,    37,     0,     0,
       0,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,     0,    21,    22,    23,    24,    25,     0,
       0,     0,    38,     0,     0,    39,    40,    26,    27,    28,
      29,    30,    31,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    32,
      33,    34,     0,     8,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    35,     0,     0,    36,    37,
       0,     0,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,     0,    21,    22,    23,    24,    25,
     228,     0,     0,     0,    38,     0,     0,    39,    40,    27,
      28,    29,    30,    31,     0,     0,     0,     0,     0,     0,
       0,     0,   630,     0,     0,     0,     0,     0,     0,     0,
      32,    33,    34,     0,     8,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    35,     0,     0,    36,
      37,     0,     0,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,     0,    21,    22,    23,    24,
      25,   452,   453,   454,   455,    38,   456,     0,     0,    40,
      27,    28,    29,    30,    31,     0,     0,     0,     0,   457,
     458,   459,   460,   461,   462,   463,   464,   465,   466,   467,
     468,    32,    33,    34,     0,     8,     0,     0,     0,   632,
       0,     0,     0,     0,     0,     0,     0,    35,   248,     0,
      36,    37,     0,     0,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,     0,    21,    22,    23,
      24,    25,     0,     0,     0,     0,    38,     0,     0,     0,
      40,    27,    28,    29,    30,    31,     0,     0,   452,   453,
     454,   455,     0,   456,     0,     0,     0,     0,     0,     0,
       0,     0,    32,    33,    34,     8,   457,   458,   459,   460,
     461,   462,   463,   464,   465,   466,   467,   468,    35,     0,
       0,    36,    37,     0,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,     0,    21,    22,    23,
      24,    25,     0,     0,     0,     0,     0,    38,     0,     0,
       0,    40,     0,     0,    30,    31,     0,     0,     0,     0,
       0,     0,     0,     0,   750,     0,     0,     0,     0,     0,
       0,     0,    32,    33,    34,     8,   756,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    35,     0,
       0,    36,    37,     0,     9,    10,    11,    12,    13,    14,
      15,    16,   703,    18,   704,    20,     0,   705,    22,    23,
      24,    25,     0,   452,   453,   454,   455,    38,   456,     0,
       0,    40,     0,     0,     0,   452,   453,   454,   455,     0,
     456,   457,   458,   459,   460,   461,   462,   463,   464,   465,
     466,   467,   468,   457,   458,   459,   460,   461,   462,   463,
     464,   465,   466,   467,   468,     0,     0,     0,    35,     0,
       0,    36,    37,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    38,     0,     0,
       0,    40
};

static const yytype_int16 yycheck[] =
{
       0,     5,   156,     5,   361,     5,   537,    44,   138,    46,
     154,    45,     5,    92,    70,   237,     5,     5,   590,   531,
     222,    43,   146,     1,     2,    40,   734,   425,   551,   671,
     148,   375,   383,   522,     1,     2,    92,    93,   240,   563,
     706,     3,   531,     6,     3,    45,   857,    46,     5,    40,
       1,     6,     5,   177,   178,    41,    54,     6,   952,     0,
     954,     5,    41,     5,    46,    98,    42,     5,    40,    69,
     259,    45,     5,     5,    74,    75,    40,   117,    76,     6,
       5,     4,   138,     6,     5,    57,   230,     5,     5,    40,
     187,    46,    59,   666,   802,    40,   193,    46,     5,     3,
       5,   674,   199,   160,   161,   138,     5,   204,     4,   109,
       6,     3,   111,   170,   147,   152,     3,    41,    21,    59,
       5,     5,    85,   105,   106,   162,     5,   700,   104,     5,
      89,    90,   166,    42,   168,   126,   233,   116,   129,     3,
     126,   103,   715,   129,   159,   136,   243,    43,   148,   672,
      46,   295,  1046,   964,   343,   118,   196,   190,    85,   118,
     119,     3,    43,   296,   297,   129,   166,   300,   168,    77,
     148,     6,   316,   881,    82,     4,   520,     6,   576,    40,
     154,   148,    85,    54,   184,    89,    90,   154,   162,    42,
     230,   542,   116,  1042,   902,   104,   540,    89,    90,    48,
      52,    54,    89,    90,    46,     6,   872,   361,   248,    54,
     250,   115,    49,    50,    51,   119,     3,    46,   114,    71,
       4,     4,   103,   115,    59,    89,    90,   119,    89,    90,
     230,    43,   232,   233,    46,     6,  1047,   237,   205,   296,
     297,   208,  1091,   300,   136,   116,  1057,    89,    90,   249,
     823,   251,   286,    54,     3,   119,   230,    74,   103,   237,
      54,   228,    46,    46,    76,   205,   240,   103,   208,   236,
     237,   979,    43,   115,   407,   408,   409,   119,   335,   336,
     116,   117,   924,    44,   284,    48,   286,   811,   228,    43,
      43,   103,   109,   816,    52,    53,   236,   117,    40,   136,
      54,  1073,    89,    90,   140,    40,   818,    48,   706,   103,
     389,    46,   286,    71,    72,    57,    77,   715,   374,   521,
     140,   295,     3,    76,     3,   117,   360,   383,   295,   818,
     448,   553,   119,   389,  1106,    43,   140,    40,    46,  1005,
      89,    90,   316,   103,     3,   103,    40,   549,   140,   316,
     483,   484,   326,   926,    57,   699,  1022,    40,    41,     3,
     360,    40,   329,    57,    40,   332,   115,   345,    76,    48,
     119,    42,    48,  1081,    57,   375,    47,   126,   345,   103,
     129,    40,   438,    43,   120,   359,    46,   136,   425,   329,
       3,    40,   332,   537,    54,    43,     3,    46,    57,     6,
     972,   401,    43,   436,    43,   928,    43,    46,    89,    90,
      89,    90,    40,   470,   471,   472,   814,   990,     4,   434,
       6,   421,   479,   480,   557,   425,   999,    40,    84,   429,
      89,    90,   489,    40,   531,    48,   115,   118,   119,   413,
     119,    48,    52,    53,    57,    89,    90,    40,    92,     4,
     424,     6,    40,   510,     6,    43,   115,   424,    46,   138,
     119,    71,    72,   437,    43,    40,   997,    46,    40,    41,
     448,   115,    84,    85,   872,   119,    89,    90,    40,   138,
      40,   448,    89,    90,   424,    57,   542,     3,    43,  1062,
     530,    46,    43,   103,   534,    46,  1069,   537,    40,    54,
      40,    43,   115,    54,    43,    40,   119,    46,   115,   866,
      40,     3,   119,   126,   105,   106,   129,   517,   558,   126,
     520,   488,   129,   136,    40,   138,   493,    41,    42,   136,
      43,   138,    48,    46,   538,    40,   538,   537,   538,   541,
     540,   541,   576,   565,   578,   538,   546,    40,   488,   538,
     538,   588,    40,   493,   935,   533,   590,   938,   615,    40,
      40,    41,    48,   537,     3,    48,   533,   769,   770,    48,
       1,    48,   550,    89,    90,   553,   576,    57,   578,   579,
     116,   538,    41,   550,    76,   538,   553,    40,    41,    43,
     590,    42,   592,     6,   538,    43,   538,    89,    90,   115,
     538,    41,    42,   119,    57,   538,   538,  1005,    54,    41,
      42,    42,    43,   538,   588,    46,   590,   538,    40,    41,
     538,   538,   138,   115,  1022,   681,    46,   119,    46,   666,
      76,   538,   672,   538,    80,    57,    42,   674,   116,   538,
      48,    48,    88,    89,    90,   649,    41,   649,   104,   649,
      89,    90,   104,   538,   538,   779,   649,   657,    42,   538,
     649,   649,   538,   700,    52,    53,   666,    40,     7,   706,
      41,  1069,    40,    41,   674,   679,   115,   679,   715,   679,
     119,   811,   722,    71,    72,    41,   679,   116,    48,    57,
     679,   679,   649,   727,    57,   697,   649,   697,   698,   699,
     700,    48,   736,   740,   697,   649,   706,   649,   697,   697,
     714,   649,   866,    40,   714,   715,   649,   649,   685,   741,
     838,   943,   679,   820,   649,    48,   679,   727,   649,    48,
     116,   649,   649,   136,   734,   679,   736,   679,    43,    41,
       3,   679,   649,     6,   649,   685,   679,   679,    41,   716,
     649,    41,   831,  1110,   679,   811,    41,    46,   679,    42,
      54,   679,   679,    41,   649,   649,   740,   741,    41,    68,
     649,    43,   679,   649,   679,   831,   716,    41,   811,   819,
     679,    41,    41,   907,    41,    41,   823,    41,   828,   121,
     122,   123,    91,   104,   679,   679,   852,    96,    42,   116,
     679,    41,   802,   679,   958,    80,    21,    40,    43,   116,
     843,   111,    46,    88,    89,    90,    49,    50,    51,   852,
      48,    54,    48,   823,    76,   116,    89,    90,   116,    44,
      49,    50,    51,   889,    48,   872,    41,    41,   838,   134,
     135,   136,   105,   106,    46,    48,   813,    80,    48,    48,
     854,    48,   854,   997,   854,    88,    89,    90,    48,   915,
     838,   854,    77,    78,    79,   854,   854,    82,   868,    84,
      85,   838,   872,   813,   874,    43,    43,   917,    73,    74,
      75,   881,    41,    43,    40,    47,   942,   927,   187,   926,
    1085,   858,    43,   192,   193,    91,    57,   854,   865,    41,
     199,   854,   902,    40,   944,   204,    90,  1102,  1103,  1104,
     854,    89,   854,    78,     3,   955,   854,    46,   858,    41,
    1115,   854,   854,   222,    48,   865,   926,    41,   968,   854,
      48,    48,    48,   854,   233,   234,   854,   854,   972,    48,
     239,   240,    48,   943,   243,    48,  1100,   854,   952,   854,
     954,    40,   952,   990,   954,   854,  1110,   997,    48,    48,
      42,    42,   999,    40,    43,   943,   966,   967,  1005,   854,
     854,    54,   972,   103,   140,   854,   943,    41,   854,   979,
      47,    42,    62,  1023,    41,  1022,    41,    41,    40,    48,
     990,    76,    41,   138,    41,    48,    45,   997,   972,   999,
      89,    90,    48,    52,    48,  1005,    48,    46,   982,    40,
     984,    76,    47,    76,   981,    47,  1072,   984,    43,    41,
       3,    54,  1022,   997,    43,  1062,   115,    54,    76,    41,
     119,    48,  1069,    47,    40,    48,    85,    86,    76,    76,
      76,   981,  1046,    41,   984,  1082,  1046,     3,  1085,   138,
      76,    42,    76,    43,    70,     3,  1056,    40,     6,    43,
      41,  1028,  1062,  1030,   104,  1102,  1103,  1104,   117,  1069,
      86,    41,    54,    48,    57,    43,    47,    40,  1115,    43,
      42,  1081,  1082,    43,    40,  1085,    43,    43,  1028,   104,
    1030,    43,    48,    41,    43,    48,   145,  1064,  1065,  1066,
      41,    48,  1102,  1103,  1104,   154,    89,    90,    47,    41,
      40,   160,   161,    40,   163,  1115,    41,    43,    40,    48,
      43,   170,  1089,    41,  1064,  1065,  1066,     3,    48,     3,
      41,    48,   115,    89,    90,    48,   119,    41,    41,    41,
      46,    89,    90,   126,    41,   522,   129,   196,    48,  1089,
      48,   200,    43,    43,   546,   138,   697,   105,   106,   115,
    1023,   943,   553,   119,    40,   214,    40,   698,   697,   697,
     126,   187,    48,   129,    48,   697,   192,   193,   592,   517,
     136,   230,   138,   199,   838,   201,   202,   448,   204,   238,
      50,   240,   241,   242,   563,   244,  1003,  1056,   429,   248,
     249,   250,   251,   219,   874,   967,   222,   223,   132,   133,
     134,   135,   136,    89,    90,    89,    90,   233,     3,   518,
     571,     3,   521,   522,   240,  1069,  1051,   243,   178,    -1,
     529,   530,   531,   249,    -1,   682,    -1,   286,    -1,   115,
     256,   115,    -1,   119,    -1,   119,   295,   296,   297,    -1,
     549,   300,   551,    -1,    -1,    40,     3,    -1,    40,   558,
      42,    -1,   138,    48,   138,    -1,    48,   316,    -1,   318,
      -1,    -1,    54,    -1,    -1,    57,     3,     4,    -1,     6,
      -1,    -1,    -1,    -1,    -1,    -1,   335,   336,    -1,    -1,
      -1,   340,    -1,    40,    76,    -1,    43,    -1,    -1,   157,
     158,    48,    -1,   352,    89,    90,    -1,    89,    90,    -1,
     359,    -1,    -1,    40,    -1,    -1,    43,    -1,    -1,    46,
      -1,    48,   104,   161,    -1,    -1,    -1,   376,    -1,    -1,
     115,    -1,   170,   115,   119,    -1,    -1,   119,    -1,     3,
      -1,     3,    89,    90,   126,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,   138,   136,    -1,   138,     3,   407,   408,
     409,    -1,    89,    90,   413,    -1,   415,   416,   115,   418,
      -1,    -1,   119,   672,    -1,   424,    40,    -1,    40,   126,
      -1,   680,   129,   682,    48,    -1,    48,   114,   115,   136,
      -1,   138,   119,    57,    40,    57,    -1,    -1,    -1,   126,
      -1,    -1,   129,    -1,   242,    -1,   244,    -1,    -1,   136,
      -1,   138,    -1,    -1,   272,    -1,   274,   275,   276,   277,
     278,   279,   280,   472,    -1,    89,    90,    89,    90,    -1,
      -1,    -1,    -1,    -1,   483,   484,    -1,    -1,    -1,    -1,
     489,    -1,    -1,    89,    90,    -1,    -1,    -1,    -1,   307,
       3,   115,     3,   115,    -1,   119,    -1,   119,    -1,    -1,
      -1,   510,   126,    -1,   513,   129,    -1,    -1,    -1,   115,
     769,   770,   330,   119,   138,   333,   138,    -1,    -1,    -1,
      -1,   530,    -1,    -1,    -1,   534,    -1,    40,   537,    40,
      -1,    -1,   138,    -1,    -1,    48,    -1,   335,   336,    -1,
      -1,    54,   340,    -1,    57,   521,    -1,    -1,   557,   558,
     559,    -1,    -1,   562,   563,   531,    -1,   816,    -1,   818,
      -1,   820,    -1,    76,    -1,    -1,    -1,   107,   108,   109,
     110,   547,     3,   549,    -1,   584,    89,    90,    89,    90,
      -1,   590,    -1,   842,    -1,     3,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,    -1,    -1,    -1,
      -1,    -1,   115,    -1,   115,    -1,   119,    -1,   119,    40,
      -1,    -1,    -1,   126,   623,   624,   129,   415,   416,   628,
     418,    -1,    40,   136,   633,   138,    57,   138,    -1,   638,
      48,    -1,    -1,    -1,   452,   453,   454,   455,   456,   457,
     458,   459,   460,   461,   462,   463,   464,   465,   466,   467,
     468,    -1,    -1,    -1,   107,   108,   109,   110,    89,    90,
      -1,    -1,     3,   672,    -1,    -1,    -1,   485,    -1,   928,
      -1,    89,    90,    -1,    -1,    -1,   494,   130,   131,   132,
     133,   134,   135,   136,   115,    -1,    -1,   696,   119,     3,
      -1,   489,    -1,   702,    -1,   126,    -1,   115,   129,    40,
      -1,   119,    43,    -1,    -1,   681,    -1,    48,   126,    -1,
      -1,   129,   510,   722,    -1,   513,    -1,    -1,   136,    -1,
     138,    -1,    -1,    -1,    -1,    -1,    40,    -1,    -1,    43,
      -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,    -1,    -1,
      49,    50,    51,    -1,    -1,     3,    -1,    -1,    89,    90,
     759,   760,    -1,    -1,    -1,    -1,   765,    -1,    -1,    -1,
      -1,   559,    -1,     3,   562,    -1,    -1,   585,   586,   587,
     588,    80,    -1,    -1,   115,    89,    90,    -1,   119,    88,
      89,    90,    40,    -1,    -1,   126,    -1,    -1,   129,    -1,
      48,    -1,    -1,   769,   770,   136,    -1,   138,    -1,    -1,
      40,   115,   811,    -1,    -1,   119,    -1,    -1,    48,    -1,
     819,    -1,   126,    -1,    -1,   129,    -1,    57,    -1,   828,
      -1,    -1,   136,    -1,   138,   623,   624,    -1,    -1,     3,
     628,    89,    90,    -1,    -1,   633,    -1,    -1,    -1,    -1,
     638,    -1,    -1,    -1,   820,    -1,    -1,    -1,    -1,    89,
      90,    -1,    -1,    -1,    -1,    -1,     3,   115,   834,   835,
     836,   119,    -1,    -1,    -1,    -1,    40,    -1,   126,   878,
      -1,   129,    -1,    -1,    48,   115,    -1,    -1,   136,   119,
     138,    -1,    -1,    57,    -1,   894,   862,    -1,    -1,   898,
     899,    -1,    -1,    40,    -1,    -1,    -1,    -1,   138,   717,
      -1,    48,    -1,    -1,   702,    -1,    -1,    -1,   917,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    90,    -1,   927,    -1,
     738,   739,   740,    -1,   742,    -1,    -1,    -1,    -1,    -1,
     748,    -1,    -1,    -1,    -1,   944,    -1,    -1,    -1,    -1,
      -1,   115,    89,    90,    -1,   119,   955,    -1,    -1,    -1,
     959,    -1,   126,    -1,    -1,   129,    -1,    -1,    -1,   968,
      -1,   759,   760,   972,   138,   974,   975,   765,   115,   107,
     108,    -1,   119,    -1,    -1,   984,    -1,    -1,    -1,   126,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,   997,   136,
      -1,   138,   130,   131,   132,   133,   134,   135,   136,   130,
     131,   132,   133,   134,   135,   136,    -1,    -1,    -1,     1,
    1019,     3,     4,    -1,  1023,    -1,     8,     9,    10,    -1,
      -1,    -1,    -1,    49,    50,    51,    -1,  1003,    54,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,  1051,    35,    36,    37,    38,    39,    41,    -1,
      76,    43,    44,    -1,    80,    47,    -1,    49,    50,    51,
      52,    53,    88,    89,    90,    -1,    -1,    -1,   886,   887,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    -1,    77,    78,    79,    -1,    81,
      82,    83,    84,    85,    86,    87,   894,    89,    90,    91,
     898,   899,    -1,    95,    -1,    97,    98,    99,   100,   101,
     102,    -1,    -1,    -1,   107,   108,   109,   110,    -1,   112,
      -1,   113,    -1,   115,    -1,    -1,   118,   119,   120,    -1,
      -1,    -1,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   134,   135,   136,    -1,    -1,   138,     3,    -1,    -1,
       6,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      -1,    31,    -1,    33,    -1,   983,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    -1,   974,   975,     3,    -1,
      -1,     6,     7,    -1,  1002,    -1,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
      35,    36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,     6,
      55,    -1,    -1,    89,    90,    -1,    13,    14,    15,    16,
      17,    18,    19,    20,    -1,    70,    71,    72,    -1,   105,
     106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
       3,    86,    -1,   119,    89,    90,    -1,    -1,    -1,    -1,
     126,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,    -1,    -1,   118,   119,    -1,    -1,    40,    -1,    -1,
      -1,   126,    -1,    -1,   129,    48,     3,   132,   133,     6,
       7,   136,   137,   138,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,    40,    -1,    -1,    89,    90,    -1,    -1,
      -1,    -1,    49,    50,    51,    52,    53,    -1,    55,    -1,
      -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,    70,    71,    72,   119,    -1,    -1,    -1,
      -1,    -1,    -1,   126,    -1,    -1,   129,    -1,     3,    86,
      -1,     3,    89,    90,     6,   138,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    19,    20,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,
      -1,    -1,   119,    -1,    -1,    40,    -1,    -1,    -1,   126,
      -1,    -1,   129,    48,     3,   132,   133,     6,     7,   136,
     137,   138,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    38,
      39,    40,    -1,    -1,    89,    90,    76,    89,    90,    -1,
      49,    50,    51,    52,    53,    -1,    55,    -1,    -1,    58,
      -1,    -1,    -1,   105,   106,    -1,    -1,    -1,    -1,    -1,
     115,    70,    71,    72,   119,    -1,    -1,   107,   108,   109,
     110,   126,   112,    -1,   129,    -1,     3,    86,    -1,    -1,
      89,    90,    -1,   138,    -1,   125,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,    -1,
     119,    -1,    -1,    40,    -1,    -1,    -1,   126,    -1,    -1,
     129,    48,     3,   132,   133,     6,     7,   136,   137,   138,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    -1,    35,    36,    37,    38,    39,    40,
      -1,    -1,    89,    90,    -1,    -1,    -1,    -1,    49,    50,
      51,    52,    53,    -1,    55,    -1,    -1,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    70,
      71,    72,   119,    -1,    -1,   107,   108,   109,   110,   126,
     112,    -1,   129,    -1,    -1,    86,    -1,    -1,    89,    90,
      -1,   138,    -1,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   134,   135,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,    -1,    -1,    -1,   119,    -1,
      -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,   129,    -1,
       3,   132,   133,     6,     7,   136,   137,   138,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    -1,    35,    36,    37,    38,    39,    40,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    50,    51,    52,
      53,    -1,    55,    -1,    -1,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
     107,   108,   109,   110,    -1,    -1,    -1,    -1,    -1,   107,
     108,   109,   110,    86,    -1,    -1,    89,    90,   125,   126,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     128,   129,   130,   131,   132,   133,   134,   135,   136,    -1,
      -1,    -1,   115,    -1,    -1,    41,   119,    -1,    -1,    -1,
      -1,    -1,    -1,   126,    -1,    -1,   129,    -1,     3,   132,
     133,     6,     7,   136,   137,   138,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
      35,    36,    37,    38,    39,    40,    41,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,    -1,
      55,   107,   108,   109,   110,    -1,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    -1,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,    86,    -1,    -1,    89,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,    -1,    -1,    -1,   119,    43,    -1,    -1,    -1,    -1,
      -1,   126,    -1,    -1,   129,    -1,     3,   132,   133,     6,
       7,   136,   137,   138,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,    40,    41,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    49,    50,    51,    52,    53,    -1,    55,   107,
     108,   109,   110,    -1,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    -1,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,    86,
      -1,    -1,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,
      -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,   126,
      -1,    -1,   129,    -1,     3,   132,   133,     6,     7,   136,
     137,   138,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    38,
      39,    40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    50,    51,    52,    53,    -1,    55,   107,   108,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    -1,    -1,    -1,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,    86,    -1,    -1,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,    -1,
     119,    -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,
     129,    -1,     3,   132,   133,     6,     7,   136,   137,   138,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    -1,    35,    36,    37,    38,    39,    40,
      -1,     3,    -1,     3,    -1,    -1,    -1,    -1,    49,    50,
      51,    52,    53,    -1,    55,    -1,     3,    -1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    -1,    -1,    -1,    -1,    -1,    -1,    40,    -1,
      40,    -1,    -1,    -1,    -1,    86,    48,    -1,    89,    90,
      -1,    -1,    -1,    40,    -1,    -1,    40,    57,    -1,    -1,
      -1,    48,    -1,    -1,    48,    -1,    -1,    -1,    -1,    -1,
      57,    -1,    -1,    57,   115,    -1,    -1,    -1,   119,    -1,
      -1,    -1,    -1,    -1,    21,    -1,    -1,    89,    90,    89,
      90,   132,   133,    -1,    -1,   136,   137,   138,    -1,    -1,
      -1,    -1,    89,    90,    -1,    89,    90,    44,    -1,    -1,
      -1,    -1,    -1,   115,    -1,   115,    -1,   119,    -1,   119,
      -1,    -1,    -1,    -1,   126,    -1,   126,   129,   115,   129,
      -1,   115,   119,    -1,   136,   119,   138,     3,   138,    -1,
      77,    78,    79,    -1,    -1,    82,    83,    84,    85,    -1,
      -1,   138,    -1,    -1,   138,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    -1,    35,
      36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,    -1,
      -1,   118,    48,    49,    50,    51,    52,    53,    -1,    -1,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,   107,   108,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      86,    -1,    -1,    89,    90,    -1,    -1,    -1,    -1,   129,
     130,   131,   132,   133,   134,   135,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,   118,   119,     3,    -1,    -1,    -1,    -1,    -1,
     126,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
     136,    -1,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    38,
      39,    40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    -1,    -1,    -1,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    -1,    -1,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,
      89,    90,    -1,    -1,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,    -1,    -1,    -1,   115,    -1,    -1,   118,
     119,    48,    49,    50,    51,    52,    53,   126,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,
      -1,    -1,    89,    90,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,   115,    -1,
      -1,   118,   119,    -1,    -1,    52,    53,    -1,    -1,   126,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    70,    71,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,
      -1,    -1,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     0,     1,    -1,     3,     4,
      -1,    -1,    -1,     8,     9,    10,    -1,    -1,   115,    -1,
      -1,    -1,   119,    -1,    -1,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,   136,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    43,    44,
      -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,    -1,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    -1,    -1,
      -1,    -1,    77,    78,    79,    -1,    81,    82,    -1,    84,
      85,    86,    87,    -1,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,     1,    -1,
       3,     4,    -1,    -1,    -1,     8,     9,    10,   113,    -1,
     115,    -1,    -1,   118,   119,    -1,    -1,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    -1,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      43,    44,    -1,    -1,    47,    -1,    49,    50,    51,    52,
      53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    -1,    -1,    -1,    77,    78,    79,    -1,    81,    82,
      -1,    84,    85,    86,    87,    -1,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
       1,    -1,     3,     4,    -1,    -1,    -1,     8,     9,    10,
     113,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    -1,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    43,    44,    -1,    -1,    47,    -1,    49,    50,
      51,    52,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    -1,    -1,    -1,    -1,    77,    78,    79,    -1,
      81,    82,    -1,    84,    85,    86,    87,    -1,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,     1,    -1,     3,     4,    -1,    -1,    -1,     8,
       9,    10,   113,    -1,   115,    -1,    -1,   118,   119,    -1,
      -1,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    43,    44,    -1,    -1,    47,    -1,
      49,    50,    51,    52,    53,    -1,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    -1,    -1,    -1,    -1,    77,    78,
      79,    -1,    81,    82,    -1,    84,    85,    86,    87,    -1,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,     1,    -1,     3,     4,    -1,    -1,
      -1,     8,     9,    10,   113,    -1,   115,    -1,    -1,   118,
     119,    -1,    -1,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    43,    44,    -1,    -1,
      -1,    -1,    49,    50,    51,    52,    53,    -1,    -1,    56,
      -1,    -1,    -1,    60,    -1,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    -1,    -1,    -1,    -1,
      77,    78,    79,    -1,    81,    82,    -1,    84,    85,    86,
      87,    -1,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     1,    -1,     3,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,    -1,
      -1,   118,   119,    -1,    -1,    -1,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,     1,    -1,
       3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    86,    -1,    -1,    89,    90,    -1,    -1,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    -1,    35,    36,    37,    38,    39,    -1,    -1,    -1,
     115,    -1,    -1,   118,   119,    48,    49,    50,    51,    52,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
       1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    86,    -1,    -1,    89,    90,    -1,    -1,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    -1,    35,    36,    37,    38,    39,    -1,
      -1,    -1,   115,    -1,    -1,   118,   119,    -1,    49,    50,
      51,    52,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    89,    90,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    -1,    35,    36,    37,    38,    39,
     107,   108,   109,   110,   115,   112,    -1,    -1,   119,    49,
      50,    51,    52,    53,    -1,    -1,    -1,    -1,   125,   126,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
      70,    71,    72,    -1,    -1,     3,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    86,    87,    -1,    89,
      90,    -1,    -1,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    -1,    35,    36,    37,
      38,    39,    -1,    -1,    -1,   115,    -1,    -1,    -1,   119,
     120,    49,    50,    51,    52,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    -1,    -1,    -1,    -1,    -1,
       3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    87,
      13,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    -1,    35,    36,    37,    38,    39,   115,    -1,    -1,
      -1,   119,   120,    -1,    -1,    48,    49,    50,    51,    52,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    86,    -1,    -1,    89,    90,    -1,    -1,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    -1,    35,    36,    37,    38,    39,    -1,
      -1,    -1,   115,    -1,    -1,   118,   119,    48,    49,    50,
      51,    52,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    89,    90,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    -1,    35,    36,    37,    38,    39,
      40,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,    49,
      50,    51,    52,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    -1,     3,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    89,
      90,    -1,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    38,
      39,   107,   108,   109,   110,   115,   112,    -1,    -1,   119,
      49,    50,    51,    52,    53,    -1,    -1,    -1,    -1,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,    70,    71,    72,    -1,     3,    -1,    -1,    -1,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    87,    -1,
      89,    90,    -1,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    -1,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,   115,    -1,    -1,    -1,
     119,    49,    50,    51,    52,    53,    -1,    -1,   107,   108,
     109,   110,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,     3,   125,   126,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,    86,    -1,
      -1,    89,    90,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    -1,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
      -1,   119,    -1,    -1,    52,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,     3,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,
      -1,    89,    90,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    -1,    35,    36,    37,
      38,    39,    -1,   107,   108,   109,   110,   115,   112,    -1,
      -1,   119,    -1,    -1,    -1,   107,   108,   109,   110,    -1,
     112,   125,   126,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   134,   135,   136,    -1,    -1,    -1,    86,    -1,
      -1,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
      -1,   119
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   121,   122,   123,   142,   143,   309,     1,     3,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    35,    36,    37,    38,    39,    48,    49,    50,    51,
      52,    53,    70,    71,    72,    86,    89,    90,   115,   118,
     119,   193,   236,   250,   251,   253,   254,   255,   256,   257,
     258,   283,   284,   294,   297,   299,     1,   236,     1,    40,
       0,     1,     4,     8,     9,    10,    21,    43,    44,    56,
      62,    63,    64,    65,    66,    67,    68,    69,    77,    78,
      79,    81,    82,    84,    85,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   113,   118,   144,
     145,   146,   148,   149,   150,   151,   152,   155,   156,   158,
     159,   160,   161,   162,   163,   164,   167,   168,   169,   172,
     174,   179,   180,   181,   182,   184,   188,   195,   196,   197,
     198,   199,   203,   204,   211,   212,   223,   231,   232,   309,
      48,    52,    71,    48,    48,    40,   140,   103,   103,   293,
     297,    43,   254,   250,    40,    48,    54,    57,    76,   119,
     126,   129,   136,   138,   241,   242,   244,   246,   247,   248,
     249,   297,   309,   250,   257,   297,   293,   117,   140,   298,
      43,    43,   233,   234,   236,   309,   120,    40,     6,    85,
     118,   303,    40,   306,   309,     1,   252,   253,   294,    40,
     306,    40,   166,   309,    40,    40,    84,    85,    40,    84,
      77,    82,    44,    77,    92,   297,    46,   294,   297,    40,
       4,    46,    40,    40,    43,    46,     4,   303,    40,   178,
     252,   176,   178,    40,    40,   303,    40,   103,   284,   306,
      40,   126,   129,   244,   249,   297,    21,    85,    87,   193,
     252,   284,    48,    48,    48,   297,   118,   119,   299,   300,
     284,     7,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    40,    55,   126,   129,   132,   133,   136,   137,
     138,   236,   237,   238,   240,   252,   253,   269,   270,   271,
     272,   303,   304,   309,   250,    40,   126,   129,   233,   247,
     249,   297,    48,    46,   105,   106,   259,   260,   261,   262,
     263,    58,   269,   270,   269,     3,    40,    48,   138,   245,
     248,   297,    48,   245,   248,   249,   250,   297,   241,    40,
      57,   241,    40,    57,    48,   126,   129,   245,   248,   297,
     116,   299,   119,   300,    41,    42,   235,   309,   261,   294,
     295,   303,   284,     6,    46,   295,   307,   295,    43,    40,
     244,    54,    41,   295,   297,   294,   294,   295,    13,   173,
     233,   233,   297,    43,    54,   214,    54,    46,   294,   175,
     307,   294,   233,    46,   243,   244,   247,   309,    43,    42,
     177,   309,   295,   296,   309,   153,   154,   303,   233,   207,
     208,   209,   236,   283,   309,   297,   303,   126,   129,   249,
     294,   297,   307,    40,   295,   126,   129,   297,   116,   244,
     297,   264,   294,   309,    40,   244,    76,   275,   276,   297,
     309,    48,    48,    41,   294,   298,   104,   269,    40,    48,
     269,   269,   269,   269,   269,   269,   269,   104,    42,   239,
     309,    40,   107,   108,   109,   110,   112,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,     7,
     126,   129,   249,   297,   246,   297,   246,    41,    41,   126,
     129,   246,   297,   116,    48,    57,   269,    58,    40,   249,
     297,    48,   297,    40,    57,    48,   249,   233,    58,   269,
     233,    58,   269,    48,    48,   245,   248,    48,   245,   248,
     116,    48,   126,   129,   245,   298,    43,   236,    41,   297,
     183,    42,    54,    41,   241,   259,    41,    46,    41,    54,
      41,    42,   171,    42,    41,    41,    43,   252,   143,   297,
     213,    41,    41,    41,    41,   176,   178,    41,    41,    42,
      46,    41,   104,    42,   210,   309,    59,   116,    41,   249,
     297,    43,   116,   111,    54,    76,   194,   309,   233,   297,
      80,    88,    89,    90,   186,   241,   250,   286,   287,   277,
      46,    43,   275,   293,   284,    41,   126,   129,   136,   249,
     252,    48,   240,   269,   269,   269,   269,   269,   269,   269,
     269,   269,   269,   269,   269,   269,   269,   269,   269,   269,
     297,   116,    41,    41,    41,   116,   246,   246,   269,   233,
     245,   297,    41,   116,    48,   233,    58,   269,    48,    41,
      58,    41,    58,    48,    48,    48,    48,   126,   129,   245,
     248,    48,    48,    48,   245,   235,     4,    46,   303,   143,
     307,   153,   271,   303,   308,    43,    43,   147,     4,   165,
     303,     4,    43,    46,   114,   170,   244,   303,   305,   295,
     303,   308,    41,   236,   244,    46,   243,    47,    43,   143,
      44,   232,   176,    43,    46,    40,    47,   177,   115,   119,
     294,   301,    43,   307,   236,   170,    91,   205,   209,   157,
     244,   303,   116,    30,    32,    35,   187,   255,   256,   297,
      57,   189,   254,    43,    46,    41,    40,    40,   286,    90,
      89,     1,    42,    43,    46,   185,   241,   287,   241,    78,
     278,   279,   285,   309,   201,    46,   297,   269,    41,    41,
     136,   250,    41,   126,   129,   242,    48,   239,    76,    41,
      58,    41,    41,   245,   245,    41,    58,   245,   245,    48,
      48,    48,    48,    48,   245,    48,    48,    48,    47,    42,
      42,     1,    43,    66,    73,    74,    75,    78,    83,   138,
     148,   149,   150,   151,   155,   156,   160,   162,   164,   167,
     169,   172,   174,   179,   180,   181,   182,   199,   203,   204,
     211,   215,   219,   220,   221,   222,   223,   224,   225,   226,
     229,   232,   309,    40,   250,   287,   288,   309,    54,    41,
      42,   171,   170,   244,   288,    43,    47,   303,   252,   294,
      43,    54,   305,   233,   140,   117,   140,   302,   103,    41,
      47,   297,    44,   118,   184,   199,   203,   204,   206,   220,
     222,   224,   232,   210,   143,   288,    43,   186,    40,    46,
     190,   150,   265,   266,   309,    40,    54,   287,   288,   289,
     233,   269,   244,   241,    42,    73,    74,    75,   280,   282,
     215,   200,   241,   269,   269,   269,    41,    41,    41,    40,
     269,    41,   245,   245,    48,    48,    48,   245,    48,    48,
     307,   307,   218,    46,    76,    76,    76,   138,    40,   299,
      47,   215,    30,    32,    35,   120,   230,   252,   256,   297,
     233,   287,   170,   303,   308,    43,   244,    41,   288,    43,
     244,    43,   178,    41,   119,   294,   294,   119,   294,   237,
       4,    46,    54,   103,    87,    60,    43,   185,   233,    40,
      43,   191,   267,   294,    42,    47,   233,   259,    54,    76,
     290,   309,    41,    41,   186,   279,   297,   281,    47,   215,
     269,   269,   252,   245,    48,    48,   245,   245,   215,   216,
     299,    40,   252,    76,    40,    43,    41,   171,   288,    43,
     244,   170,    43,    43,   302,   302,   104,   252,   207,    41,
     192,   265,    54,   265,    43,   244,    41,    43,   261,   291,
     292,   297,    43,    46,   185,    48,   273,   274,   309,   285,
      43,   202,   244,    47,   242,   245,   245,   215,    40,   233,
      40,   126,   129,   249,   269,   233,    43,    43,   288,    43,
     243,   104,   288,    43,   268,   269,   267,   186,    43,    46,
      43,    42,    48,    40,    46,    48,   297,   186,   202,    41,
      47,   233,    41,   233,    40,    40,    40,   129,    43,    41,
      43,    43,   111,   190,   265,   185,   292,    48,    48,   274,
     185,   217,    41,   227,   288,    41,   233,   233,   233,    40,
     289,   252,   191,    48,    48,   215,   228,   288,    43,    46,
      54,   228,    41,    41,    41,   233,   190,    48,    43,    46,
      54,   261,   228,   228,   228,    41,   191,    48,   259,    43,
     228,    43
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1464 of yacc.c  */
#line 1511 "parser.y"
    {
                   if (!classes) classes = NewHash();
		   Setattr((yyvsp[(1) - (1)].node),"classes",classes); 
		   Setattr((yyvsp[(1) - (1)].node),"name",ModuleName);
		   
		   if ((!module_node) && ModuleName) {
		     module_node = new_node("module");
		     Setattr(module_node,"name",ModuleName);
		   }
		   Setattr((yyvsp[(1) - (1)].node),"module",module_node);
	           top = (yyvsp[(1) - (1)].node);
               }
    break;

  case 3:

/* Line 1464 of yacc.c  */
#line 1523 "parser.y"
    {
                 top = Copy(Getattr((yyvsp[(2) - (3)].p),"type"));
		 Delete((yyvsp[(2) - (3)].p));
               }
    break;

  case 4:

/* Line 1464 of yacc.c  */
#line 1527 "parser.y"
    {
                 top = 0;
               }
    break;

  case 5:

/* Line 1464 of yacc.c  */
#line 1530 "parser.y"
    {
                 top = (yyvsp[(2) - (3)].p);
               }
    break;

  case 6:

/* Line 1464 of yacc.c  */
#line 1533 "parser.y"
    {
                 top = 0;
               }
    break;

  case 7:

/* Line 1464 of yacc.c  */
#line 1536 "parser.y"
    {
                 top = (yyvsp[(3) - (5)].pl);
               }
    break;

  case 8:

/* Line 1464 of yacc.c  */
#line 1539 "parser.y"
    {
                 top = 0;
               }
    break;

  case 9:

/* Line 1464 of yacc.c  */
#line 1544 "parser.y"
    {  
                   /* add declaration to end of linked list (the declaration isn't always a single declaration, sometimes it is a linked list itself) */
                   appendChild((yyvsp[(1) - (2)].node),(yyvsp[(2) - (2)].node));
                   (yyval.node) = (yyvsp[(1) - (2)].node);
               }
    break;

  case 10:

/* Line 1464 of yacc.c  */
#line 1549 "parser.y"
    {
                   (yyval.node) = new_node("top");
               }
    break;

  case 11:

/* Line 1464 of yacc.c  */
#line 1554 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 12:

/* Line 1464 of yacc.c  */
#line 1555 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 13:

/* Line 1464 of yacc.c  */
#line 1556 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 14:

/* Line 1464 of yacc.c  */
#line 1557 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 15:

/* Line 1464 of yacc.c  */
#line 1558 "parser.y"
    {
                  (yyval.node) = 0;
		  if (cparse_unknown_directive) {
		      Swig_error(cparse_file, cparse_line, "Unknown directive '%s'.\n", cparse_unknown_directive);
		  } else {
		      Swig_error(cparse_file, cparse_line, "Syntax error in input(1).\n");
		  }
		  exit(1);
               }
    break;

  case 16:

/* Line 1464 of yacc.c  */
#line 1568 "parser.y"
    { 
                  if ((yyval.node)) {
   		      add_symbols((yyval.node));
                  }
                  (yyval.node) = (yyvsp[(1) - (1)].node); 
	       }
    break;

  case 17:

/* Line 1464 of yacc.c  */
#line 1584 "parser.y"
    {
                  (yyval.node) = 0;
                  skip_decl();
               }
    break;

  case 18:

/* Line 1464 of yacc.c  */
#line 1594 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 19:

/* Line 1464 of yacc.c  */
#line 1595 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 20:

/* Line 1464 of yacc.c  */
#line 1596 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 21:

/* Line 1464 of yacc.c  */
#line 1597 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 22:

/* Line 1464 of yacc.c  */
#line 1598 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 23:

/* Line 1464 of yacc.c  */
#line 1599 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 24:

/* Line 1464 of yacc.c  */
#line 1600 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 25:

/* Line 1464 of yacc.c  */
#line 1601 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 26:

/* Line 1464 of yacc.c  */
#line 1602 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 27:

/* Line 1464 of yacc.c  */
#line 1603 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 28:

/* Line 1464 of yacc.c  */
#line 1604 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 29:

/* Line 1464 of yacc.c  */
#line 1605 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 30:

/* Line 1464 of yacc.c  */
#line 1606 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 31:

/* Line 1464 of yacc.c  */
#line 1607 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 32:

/* Line 1464 of yacc.c  */
#line 1608 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 33:

/* Line 1464 of yacc.c  */
#line 1609 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 34:

/* Line 1464 of yacc.c  */
#line 1610 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 35:

/* Line 1464 of yacc.c  */
#line 1611 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 36:

/* Line 1464 of yacc.c  */
#line 1612 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 37:

/* Line 1464 of yacc.c  */
#line 1613 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 38:

/* Line 1464 of yacc.c  */
#line 1614 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 39:

/* Line 1464 of yacc.c  */
#line 1621 "parser.y"
    {
               Node *cls;
	       String *clsname;
	       extendmode = 1;
	       cplus_mode = CPLUS_PUBLIC;
	       if (!classes) classes = NewHash();
	       if (!classes_typedefs) classes_typedefs = NewHash();
	       clsname = make_class_name((yyvsp[(3) - (4)].str));
	       cls = Getattr(classes,clsname);
	       if (!cls) {
	         cls = Getattr(classes_typedefs, clsname);
		 if (!cls) {
		   /* No previous definition. Create a new scope */
		   Node *am = Getattr(Swig_extend_hash(),clsname);
		   if (!am) {
		     Swig_symbol_newscope();
		     Swig_symbol_setscopename((yyvsp[(3) - (4)].str));
		     prev_symtab = 0;
		   } else {
		     prev_symtab = Swig_symbol_setscope(Getattr(am,"symtab"));
		   }
		   current_class = 0;
		 } else {
		   /* Previous typedef class definition.  Use its symbol table.
		      Deprecated, just the real name should be used. 
		      Note that %extend before the class typedef never worked, only %extend after the class typdef. */
		   prev_symtab = Swig_symbol_setscope(Getattr(cls, "symtab"));
		   current_class = cls;
		   SWIG_WARN_NODE_BEGIN(cls);
		   Swig_warning(WARN_PARSE_EXTEND_NAME, cparse_file, cparse_line, "Deprecated %%extend name used - the %s name '%s' should be used instead of the typedef name '%s'.\n", Getattr(cls, "kind"), SwigType_namestr(Getattr(cls, "name")), (yyvsp[(3) - (4)].str));
		   SWIG_WARN_NODE_END(cls);
		 }
	       } else {
		 /* Previous class definition.  Use its symbol table */
		 prev_symtab = Swig_symbol_setscope(Getattr(cls,"symtab"));
		 current_class = cls;
	       }
	       Classprefix = NewString((yyvsp[(3) - (4)].str));
	       Namespaceprefix= Swig_symbol_qualifiedscopename(0);
	       Delete(clsname);
	     }
    break;

  case 40:

/* Line 1464 of yacc.c  */
#line 1661 "parser.y"
    {
               String *clsname;
	       extendmode = 0;
               (yyval.node) = new_node("extend");
	       Setattr((yyval.node),"symtab",Swig_symbol_popscope());
	       if (prev_symtab) {
		 Swig_symbol_setscope(prev_symtab);
	       }
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
               clsname = make_class_name((yyvsp[(3) - (7)].str));
	       Setattr((yyval.node),"name",clsname);

	       mark_nodes_as_extend((yyvsp[(6) - (7)].node));
	       if (current_class) {
		 /* We add the extension to the previously defined class */
		 appendChild((yyval.node),(yyvsp[(6) - (7)].node));
		 appendChild(current_class,(yyval.node));
	       } else {
		 /* We store the extensions in the extensions hash */
		 Node *am = Getattr(Swig_extend_hash(),clsname);
		 if (am) {
		   /* Append the members to the previous extend methods */
		   appendChild(am,(yyvsp[(6) - (7)].node));
		 } else {
		   appendChild((yyval.node),(yyvsp[(6) - (7)].node));
		   Setattr(Swig_extend_hash(),clsname,(yyval.node));
		 }
	       }
	       current_class = 0;
	       Delete(Classprefix);
	       Delete(clsname);
	       Classprefix = 0;
	       prev_symtab = 0;
	       (yyval.node) = 0;

	     }
    break;

  case 41:

/* Line 1464 of yacc.c  */
#line 1703 "parser.y"
    {
                    (yyval.node) = new_node("apply");
                    Setattr((yyval.node),"pattern",Getattr((yyvsp[(2) - (5)].p),"pattern"));
		    appendChild((yyval.node),(yyvsp[(4) - (5)].p));
               }
    break;

  case 42:

/* Line 1464 of yacc.c  */
#line 1713 "parser.y"
    {
		 (yyval.node) = new_node("clear");
		 appendChild((yyval.node),(yyvsp[(2) - (3)].p));
               }
    break;

  case 43:

/* Line 1464 of yacc.c  */
#line 1724 "parser.y"
    {
		   if (((yyvsp[(4) - (5)].dtype).type != T_ERROR) && ((yyvsp[(4) - (5)].dtype).type != T_SYMBOL)) {
		     SwigType *type = NewSwigType((yyvsp[(4) - (5)].dtype).type);
		     (yyval.node) = new_node("constant");
		     Setattr((yyval.node),"name",(yyvsp[(2) - (5)].id));
		     Setattr((yyval.node),"type",type);
		     Setattr((yyval.node),"value",(yyvsp[(4) - (5)].dtype).val);
		     if ((yyvsp[(4) - (5)].dtype).rawval) Setattr((yyval.node),"rawval", (yyvsp[(4) - (5)].dtype).rawval);
		     Setattr((yyval.node),"storage","%constant");
		     SetFlag((yyval.node),"feature:immutable");
		     add_symbols((yyval.node));
		     Delete(type);
		   } else {
		     if ((yyvsp[(4) - (5)].dtype).type == T_ERROR) {
		       Swig_warning(WARN_PARSE_UNSUPPORTED_VALUE,cparse_file,cparse_line,"Unsupported constant value (ignored)\n");
		     }
		     (yyval.node) = 0;
		   }

	       }
    break;

  case 44:

/* Line 1464 of yacc.c  */
#line 1745 "parser.y"
    {
		 if (((yyvsp[(4) - (5)].dtype).type != T_ERROR) && ((yyvsp[(4) - (5)].dtype).type != T_SYMBOL)) {
		   SwigType_push((yyvsp[(2) - (5)].type),(yyvsp[(3) - (5)].decl).type);
		   /* Sneaky callback function trick */
		   if (SwigType_isfunction((yyvsp[(2) - (5)].type))) {
		     SwigType_add_pointer((yyvsp[(2) - (5)].type));
		   }
		   (yyval.node) = new_node("constant");
		   Setattr((yyval.node),"name",(yyvsp[(3) - (5)].decl).id);
		   Setattr((yyval.node),"type",(yyvsp[(2) - (5)].type));
		   Setattr((yyval.node),"value",(yyvsp[(4) - (5)].dtype).val);
		   if ((yyvsp[(4) - (5)].dtype).rawval) Setattr((yyval.node),"rawval", (yyvsp[(4) - (5)].dtype).rawval);
		   Setattr((yyval.node),"storage","%constant");
		   SetFlag((yyval.node),"feature:immutable");
		   add_symbols((yyval.node));
		 } else {
		     if ((yyvsp[(4) - (5)].dtype).type == T_ERROR) {
		       Swig_warning(WARN_PARSE_UNSUPPORTED_VALUE,cparse_file,cparse_line,"Unsupported constant value\n");
		     }
		   (yyval.node) = 0;
		 }
               }
    break;

  case 45:

/* Line 1464 of yacc.c  */
#line 1767 "parser.y"
    {
		 Swig_warning(WARN_PARSE_BAD_VALUE,cparse_file,cparse_line,"Bad constant value (ignored).\n");
		 (yyval.node) = 0;
	       }
    break;

  case 46:

/* Line 1464 of yacc.c  */
#line 1778 "parser.y"
    {
		 char temp[64];
		 Replace((yyvsp[(2) - (2)].str),"$file",cparse_file, DOH_REPLACE_ANY);
		 sprintf(temp,"%d", cparse_line);
		 Replace((yyvsp[(2) - (2)].str),"$line",temp,DOH_REPLACE_ANY);
		 Printf(stderr,"%s\n", (yyvsp[(2) - (2)].str));
		 Delete((yyvsp[(2) - (2)].str));
                 (yyval.node) = 0;
	       }
    break;

  case 47:

/* Line 1464 of yacc.c  */
#line 1787 "parser.y"
    {
		 char temp[64];
		 String *s = (yyvsp[(2) - (2)].str);
		 Replace(s,"$file",cparse_file, DOH_REPLACE_ANY);
		 sprintf(temp,"%d", cparse_line);
		 Replace(s,"$line",temp,DOH_REPLACE_ANY);
		 Printf(stderr,"%s\n", s);
		 Delete(s);
                 (yyval.node) = 0;
               }
    break;

  case 48:

/* Line 1464 of yacc.c  */
#line 1806 "parser.y"
    {
                    skip_balanced('{','}');
		    (yyval.node) = 0;
		    Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
	       }
    break;

  case 49:

/* Line 1464 of yacc.c  */
#line 1812 "parser.y"
    {
                    skip_balanced('{','}');
		    (yyval.node) = 0;
		    Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
               }
    break;

  case 50:

/* Line 1464 of yacc.c  */
#line 1818 "parser.y"
    {
		 (yyval.node) = 0;
		 Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
               }
    break;

  case 51:

/* Line 1464 of yacc.c  */
#line 1823 "parser.y"
    {
		 (yyval.node) = 0;
		 Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
	       }
    break;

  case 52:

/* Line 1464 of yacc.c  */
#line 1830 "parser.y"
    {		 
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"value",(yyvsp[(1) - (4)].str));
		 Setattr((yyval.node),"type",Getattr((yyvsp[(3) - (4)].p),"type"));
               }
    break;

  case 53:

/* Line 1464 of yacc.c  */
#line 1837 "parser.y"
    {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"value",(yyvsp[(1) - (1)].str));
              }
    break;

  case 54:

/* Line 1464 of yacc.c  */
#line 1841 "parser.y"
    {
                (yyval.node) = (yyvsp[(1) - (1)].node);
              }
    break;

  case 55:

/* Line 1464 of yacc.c  */
#line 1854 "parser.y"
    {
                   Hash *p = (yyvsp[(5) - (7)].node);
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[(3) - (7)].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[(3) - (7)].node),"type"));
		   Setattr((yyval.node),"section",Getattr(p,"name"));
		   Setattr((yyval.node),"kwargs",nextSibling(p));
		   Setattr((yyval.node),"code",(yyvsp[(7) - (7)].str));
                 }
    break;

  case 56:

/* Line 1464 of yacc.c  */
#line 1863 "parser.y"
    {
		   Hash *p = (yyvsp[(5) - (7)].node);
		   String *code;
                   skip_balanced('{','}');
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[(3) - (7)].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[(3) - (7)].node),"type"));
		   Setattr((yyval.node),"section",Getattr(p,"name"));
		   Setattr((yyval.node),"kwargs",nextSibling(p));
		   Delitem(scanner_ccode,0);
		   Delitem(scanner_ccode,DOH_END);
		   code = Copy(scanner_ccode);
		   Setattr((yyval.node),"code",code);
		   Delete(code);
                 }
    break;

  case 57:

/* Line 1464 of yacc.c  */
#line 1878 "parser.y"
    {
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[(3) - (5)].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[(3) - (5)].node),"type"));
		   Setattr((yyval.node),"emitonly","1");
		 }
    break;

  case 58:

/* Line 1464 of yacc.c  */
#line 1891 "parser.y"
    {
                     (yyvsp[(1) - (4)].loc).filename = Copy(cparse_file);
		     (yyvsp[(1) - (4)].loc).line = cparse_line;
		     scanner_set_location((yyvsp[(3) - (4)].str),1);
                     if ((yyvsp[(2) - (4)].node)) { 
		       String *maininput = Getattr((yyvsp[(2) - (4)].node), "maininput");
		       if (maininput)
		         scanner_set_main_input_file(NewString(maininput));
		     }
               }
    break;

  case 59:

/* Line 1464 of yacc.c  */
#line 1900 "parser.y"
    {
                     String *mname = 0;
                     (yyval.node) = (yyvsp[(6) - (7)].node);
		     scanner_set_location((yyvsp[(1) - (7)].loc).filename,(yyvsp[(1) - (7)].loc).line+1);
		     if (strcmp((yyvsp[(1) - (7)].loc).type,"include") == 0) set_nodeType((yyval.node),"include");
		     if (strcmp((yyvsp[(1) - (7)].loc).type,"import") == 0) {
		       mname = (yyvsp[(2) - (7)].node) ? Getattr((yyvsp[(2) - (7)].node),"module") : 0;
		       set_nodeType((yyval.node),"import");
		       if (import_mode) --import_mode;
		     }
		     
		     Setattr((yyval.node),"name",(yyvsp[(3) - (7)].str));
		     /* Search for the module (if any) */
		     {
			 Node *n = firstChild((yyval.node));
			 while (n) {
			     if (Strcmp(nodeType(n),"module") == 0) {
			         if (mname) {
				   Setattr(n,"name", mname);
				   mname = 0;
				 }
				 Setattr((yyval.node),"module",Getattr(n,"name"));
				 break;
			     }
			     n = nextSibling(n);
			 }
			 if (mname) {
			   /* There is no module node in the import
			      node, ie, you imported a .h file
			      directly.  We are forced then to create
			      a new import node with a module node.
			   */			      
			   Node *nint = new_node("import");
			   Node *mnode = new_node("module");
			   Setattr(mnode,"name", mname);
                           Setattr(mnode,"options",(yyvsp[(2) - (7)].node));
			   appendChild(nint,mnode);
			   Delete(mnode);
			   appendChild(nint,firstChild((yyval.node)));
			   (yyval.node) = nint;
			   Setattr((yyval.node),"module",mname);
			 }
		     }
		     Setattr((yyval.node),"options",(yyvsp[(2) - (7)].node));
               }
    break;

  case 60:

/* Line 1464 of yacc.c  */
#line 1947 "parser.y"
    { (yyval.loc).type = "include"; }
    break;

  case 61:

/* Line 1464 of yacc.c  */
#line 1948 "parser.y"
    { (yyval.loc).type = "import"; ++import_mode;}
    break;

  case 62:

/* Line 1464 of yacc.c  */
#line 1955 "parser.y"
    {
                 String *cpps;
		 if (Namespaceprefix) {
		   Swig_error(cparse_file, cparse_start_line, "%%inline directive inside a namespace is disallowed.\n");
		   (yyval.node) = 0;
		 } else {
		   (yyval.node) = new_node("insert");
		   Setattr((yyval.node),"code",(yyvsp[(2) - (2)].str));
		   /* Need to run through the preprocessor */
		   Seek((yyvsp[(2) - (2)].str),0,SEEK_SET);
		   Setline((yyvsp[(2) - (2)].str),cparse_start_line);
		   Setfile((yyvsp[(2) - (2)].str),cparse_file);
		   cpps = Preprocessor_parse((yyvsp[(2) - (2)].str));
		   start_inline(Char(cpps), cparse_start_line);
		   Delete((yyvsp[(2) - (2)].str));
		   Delete(cpps);
		 }
		 
	       }
    break;

  case 63:

/* Line 1464 of yacc.c  */
#line 1974 "parser.y"
    {
                 String *cpps;
		 int start_line = cparse_line;
		 skip_balanced('{','}');
		 if (Namespaceprefix) {
		   Swig_error(cparse_file, cparse_start_line, "%%inline directive inside a namespace is disallowed.\n");
		   
		   (yyval.node) = 0;
		 } else {
		   String *code;
                   (yyval.node) = new_node("insert");
		   Delitem(scanner_ccode,0);
		   Delitem(scanner_ccode,DOH_END);
		   code = Copy(scanner_ccode);
		   Setattr((yyval.node),"code", code);
		   Delete(code);		   
		   cpps=Copy(scanner_ccode);
		   start_inline(Char(cpps), start_line);
		   Delete(cpps);
		 }
               }
    break;

  case 64:

/* Line 1464 of yacc.c  */
#line 2005 "parser.y"
    {
                 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"code",(yyvsp[(1) - (1)].str));
	       }
    break;

  case 65:

/* Line 1464 of yacc.c  */
#line 2009 "parser.y"
    {
		 String *code = NewStringEmpty();
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[(3) - (5)].id));
		 Setattr((yyval.node),"code",code);
		 if (Swig_insert_file((yyvsp[(5) - (5)].str),code) < 0) {
		   Swig_error(cparse_file, cparse_line, "Couldn't find '%s'.\n", (yyvsp[(5) - (5)].str));
		   (yyval.node) = 0;
		 } 
               }
    break;

  case 66:

/* Line 1464 of yacc.c  */
#line 2019 "parser.y"
    {
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[(3) - (5)].id));
		 Setattr((yyval.node),"code",(yyvsp[(5) - (5)].str));
               }
    break;

  case 67:

/* Line 1464 of yacc.c  */
#line 2024 "parser.y"
    {
		 String *code;
                 skip_balanced('{','}');
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[(3) - (5)].id));
		 Delitem(scanner_ccode,0);
		 Delitem(scanner_ccode,DOH_END);
		 code = Copy(scanner_ccode);
		 Setattr((yyval.node),"code", code);
		 Delete(code);
	       }
    break;

  case 68:

/* Line 1464 of yacc.c  */
#line 2042 "parser.y"
    {
                 (yyval.node) = new_node("module");
		 if ((yyvsp[(2) - (3)].node)) {
		   Setattr((yyval.node),"options",(yyvsp[(2) - (3)].node));
		   if (Getattr((yyvsp[(2) - (3)].node),"directors")) {
		     Wrapper_director_mode_set(1);
		     if (!cparse_cplusplus) {
		       Swig_error(cparse_file, cparse_line, "Directors are not supported for C code and require the -c++ option\n");
		     }
		   } 
		   if (Getattr((yyvsp[(2) - (3)].node),"dirprot")) {
		     Wrapper_director_protected_mode_set(1);
		   } 
		   if (Getattr((yyvsp[(2) - (3)].node),"allprotected")) {
		     Wrapper_all_protected_mode_set(1);
		   } 
		   if (Getattr((yyvsp[(2) - (3)].node),"templatereduce")) {
		     template_reduce = 1;
		   }
		   if (Getattr((yyvsp[(2) - (3)].node),"notemplatereduce")) {
		     template_reduce = 0;
		   }
		 }
		 if (!ModuleName) ModuleName = NewString((yyvsp[(3) - (3)].id));
		 if (!import_mode) {
		   /* first module included, we apply global
		      ModuleName, which can be modify by -module */
		   String *mname = Copy(ModuleName);
		   Setattr((yyval.node),"name",mname);
		   Delete(mname);
		 } else { 
		   /* import mode, we just pass the idstring */
		   Setattr((yyval.node),"name",(yyvsp[(3) - (3)].id));   
		 }		 
		 if (!module_node) module_node = (yyval.node);
	       }
    break;

  case 69:

/* Line 1464 of yacc.c  */
#line 2085 "parser.y"
    {
                 Swig_warning(WARN_DEPRECATED_NAME,cparse_file,cparse_line, "%%name is deprecated.  Use %%rename instead.\n");
		 Delete(yyrename);
                 yyrename = NewString((yyvsp[(3) - (4)].id));
		 (yyval.node) = 0;
               }
    break;

  case 70:

/* Line 1464 of yacc.c  */
#line 2091 "parser.y"
    {
		 Swig_warning(WARN_DEPRECATED_NAME,cparse_file,cparse_line, "%%name is deprecated.  Use %%rename instead.\n");
		 (yyval.node) = 0;
		 Swig_error(cparse_file,cparse_line,"Missing argument to %%name directive.\n");
	       }
    break;

  case 71:

/* Line 1464 of yacc.c  */
#line 2104 "parser.y"
    {
                 (yyval.node) = new_node("native");
		 Setattr((yyval.node),"name",(yyvsp[(3) - (7)].id));
		 Setattr((yyval.node),"wrap:name",(yyvsp[(6) - (7)].id));
	         add_symbols((yyval.node));
	       }
    break;

  case 72:

/* Line 1464 of yacc.c  */
#line 2110 "parser.y"
    {
		 if (!SwigType_isfunction((yyvsp[(7) - (8)].decl).type)) {
		   Swig_error(cparse_file,cparse_line,"%%native declaration '%s' is not a function.\n", (yyvsp[(7) - (8)].decl).id);
		   (yyval.node) = 0;
		 } else {
		     Delete(SwigType_pop_function((yyvsp[(7) - (8)].decl).type));
		     /* Need check for function here */
		     SwigType_push((yyvsp[(6) - (8)].type),(yyvsp[(7) - (8)].decl).type);
		     (yyval.node) = new_node("native");
	             Setattr((yyval.node),"name",(yyvsp[(3) - (8)].id));
		     Setattr((yyval.node),"wrap:name",(yyvsp[(7) - (8)].decl).id);
		     Setattr((yyval.node),"type",(yyvsp[(6) - (8)].type));
		     Setattr((yyval.node),"parms",(yyvsp[(7) - (8)].decl).parms);
		     Setattr((yyval.node),"decl",(yyvsp[(7) - (8)].decl).type);
		 }
	         add_symbols((yyval.node));
	       }
    break;

  case 73:

/* Line 1464 of yacc.c  */
#line 2136 "parser.y"
    {
                 (yyval.node) = new_node("pragma");
		 Setattr((yyval.node),"lang",(yyvsp[(2) - (5)].id));
		 Setattr((yyval.node),"name",(yyvsp[(3) - (5)].id));
		 Setattr((yyval.node),"value",(yyvsp[(5) - (5)].str));
	       }
    break;

  case 74:

/* Line 1464 of yacc.c  */
#line 2142 "parser.y"
    {
		(yyval.node) = new_node("pragma");
		Setattr((yyval.node),"lang",(yyvsp[(2) - (3)].id));
		Setattr((yyval.node),"name",(yyvsp[(3) - (3)].id));
	      }
    break;

  case 75:

/* Line 1464 of yacc.c  */
#line 2149 "parser.y"
    { (yyval.str) = (yyvsp[(1) - (1)].str); }
    break;

  case 76:

/* Line 1464 of yacc.c  */
#line 2150 "parser.y"
    { (yyval.str) = (yyvsp[(1) - (1)].str); }
    break;

  case 77:

/* Line 1464 of yacc.c  */
#line 2153 "parser.y"
    { (yyval.id) = (yyvsp[(2) - (3)].id); }
    break;

  case 78:

/* Line 1464 of yacc.c  */
#line 2154 "parser.y"
    { (yyval.id) = (char *) "swig"; }
    break;

  case 79:

/* Line 1464 of yacc.c  */
#line 2161 "parser.y"
    {
                SwigType *t = (yyvsp[(2) - (4)].decl).type;
		Hash *kws = NewHash();
		String *fixname;
		fixname = feature_identifier_fix((yyvsp[(2) - (4)].decl).id);
		Setattr(kws,"name",(yyvsp[(3) - (4)].id));
		if (!Len(t)) t = 0;
		/* Special declarator check */
		if (t) {
		  if (SwigType_isfunction(t)) {
		    SwigType *decl = SwigType_pop_function(t);
		    if (SwigType_ispointer(t)) {
		      String *nname = NewStringf("*%s",fixname);
		      if ((yyvsp[(1) - (4)].intvalue)) {
			Swig_name_rename_add(Namespaceprefix, nname,decl,kws,(yyvsp[(2) - (4)].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,nname,decl,kws);
		      }
		      Delete(nname);
		    } else {
		      if ((yyvsp[(1) - (4)].intvalue)) {
			Swig_name_rename_add(Namespaceprefix,(fixname),decl,kws,(yyvsp[(2) - (4)].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,(fixname),decl,kws);
		      }
		    }
		    Delete(decl);
		  } else if (SwigType_ispointer(t)) {
		    String *nname = NewStringf("*%s",fixname);
		    if ((yyvsp[(1) - (4)].intvalue)) {
		      Swig_name_rename_add(Namespaceprefix,(nname),0,kws,(yyvsp[(2) - (4)].decl).parms);
		    } else {
		      Swig_name_namewarn_add(Namespaceprefix,(nname),0,kws);
		    }
		    Delete(nname);
		  }
		} else {
		  if ((yyvsp[(1) - (4)].intvalue)) {
		    Swig_name_rename_add(Namespaceprefix,(fixname),0,kws,(yyvsp[(2) - (4)].decl).parms);
		  } else {
		    Swig_name_namewarn_add(Namespaceprefix,(fixname),0,kws);
		  }
		}
                (yyval.node) = 0;
		scanner_clear_rename();
              }
    break;

  case 80:

/* Line 1464 of yacc.c  */
#line 2207 "parser.y"
    {
		String *fixname;
		Hash *kws = (yyvsp[(3) - (7)].node);
		SwigType *t = (yyvsp[(5) - (7)].decl).type;
		fixname = feature_identifier_fix((yyvsp[(5) - (7)].decl).id);
		if (!Len(t)) t = 0;
		/* Special declarator check */
		if (t) {
		  if ((yyvsp[(6) - (7)].dtype).qualifier) SwigType_push(t,(yyvsp[(6) - (7)].dtype).qualifier);
		  if (SwigType_isfunction(t)) {
		    SwigType *decl = SwigType_pop_function(t);
		    if (SwigType_ispointer(t)) {
		      String *nname = NewStringf("*%s",fixname);
		      if ((yyvsp[(1) - (7)].intvalue)) {
			Swig_name_rename_add(Namespaceprefix, nname,decl,kws,(yyvsp[(5) - (7)].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,nname,decl,kws);
		      }
		      Delete(nname);
		    } else {
		      if ((yyvsp[(1) - (7)].intvalue)) {
			Swig_name_rename_add(Namespaceprefix,(fixname),decl,kws,(yyvsp[(5) - (7)].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,(fixname),decl,kws);
		      }
		    }
		    Delete(decl);
		  } else if (SwigType_ispointer(t)) {
		    String *nname = NewStringf("*%s",fixname);
		    if ((yyvsp[(1) - (7)].intvalue)) {
		      Swig_name_rename_add(Namespaceprefix,(nname),0,kws,(yyvsp[(5) - (7)].decl).parms);
		    } else {
		      Swig_name_namewarn_add(Namespaceprefix,(nname),0,kws);
		    }
		    Delete(nname);
		  }
		} else {
		  if ((yyvsp[(1) - (7)].intvalue)) {
		    Swig_name_rename_add(Namespaceprefix,(fixname),0,kws,(yyvsp[(5) - (7)].decl).parms);
		  } else {
		    Swig_name_namewarn_add(Namespaceprefix,(fixname),0,kws);
		  }
		}
                (yyval.node) = 0;
		scanner_clear_rename();
              }
    break;

  case 81:

/* Line 1464 of yacc.c  */
#line 2253 "parser.y"
    {
		if ((yyvsp[(1) - (6)].intvalue)) {
		  Swig_name_rename_add(Namespaceprefix,(yyvsp[(5) - (6)].str),0,(yyvsp[(3) - (6)].node),0);
		} else {
		  Swig_name_namewarn_add(Namespaceprefix,(yyvsp[(5) - (6)].str),0,(yyvsp[(3) - (6)].node));
		}
		(yyval.node) = 0;
		scanner_clear_rename();
              }
    break;

  case 82:

/* Line 1464 of yacc.c  */
#line 2264 "parser.y"
    {
		    (yyval.intvalue) = 1;
                }
    break;

  case 83:

/* Line 1464 of yacc.c  */
#line 2267 "parser.y"
    {
                    (yyval.intvalue) = 0;
                }
    break;

  case 84:

/* Line 1464 of yacc.c  */
#line 2294 "parser.y"
    {
                    String *val = (yyvsp[(7) - (7)].str) ? NewString((yyvsp[(7) - (7)].str)) : NewString("1");
                    new_feature((yyvsp[(3) - (7)].id), val, 0, (yyvsp[(5) - (7)].decl).id, (yyvsp[(5) - (7)].decl).type, (yyvsp[(5) - (7)].decl).parms, (yyvsp[(6) - (7)].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 85:

/* Line 1464 of yacc.c  */
#line 2300 "parser.y"
    {
                    String *val = Len((yyvsp[(5) - (9)].str)) ? (yyvsp[(5) - (9)].str) : 0;
                    new_feature((yyvsp[(3) - (9)].id), val, 0, (yyvsp[(7) - (9)].decl).id, (yyvsp[(7) - (9)].decl).type, (yyvsp[(7) - (9)].decl).parms, (yyvsp[(8) - (9)].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 86:

/* Line 1464 of yacc.c  */
#line 2306 "parser.y"
    {
                    String *val = (yyvsp[(8) - (8)].str) ? NewString((yyvsp[(8) - (8)].str)) : NewString("1");
                    new_feature((yyvsp[(3) - (8)].id), val, (yyvsp[(4) - (8)].node), (yyvsp[(6) - (8)].decl).id, (yyvsp[(6) - (8)].decl).type, (yyvsp[(6) - (8)].decl).parms, (yyvsp[(7) - (8)].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 87:

/* Line 1464 of yacc.c  */
#line 2312 "parser.y"
    {
                    String *val = Len((yyvsp[(5) - (10)].str)) ? (yyvsp[(5) - (10)].str) : 0;
                    new_feature((yyvsp[(3) - (10)].id), val, (yyvsp[(6) - (10)].node), (yyvsp[(8) - (10)].decl).id, (yyvsp[(8) - (10)].decl).type, (yyvsp[(8) - (10)].decl).parms, (yyvsp[(9) - (10)].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 88:

/* Line 1464 of yacc.c  */
#line 2320 "parser.y"
    {
                    String *val = (yyvsp[(5) - (5)].str) ? NewString((yyvsp[(5) - (5)].str)) : NewString("1");
                    new_feature((yyvsp[(3) - (5)].id), val, 0, 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 89:

/* Line 1464 of yacc.c  */
#line 2326 "parser.y"
    {
                    String *val = Len((yyvsp[(5) - (7)].str)) ? (yyvsp[(5) - (7)].str) : 0;
                    new_feature((yyvsp[(3) - (7)].id), val, 0, 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 90:

/* Line 1464 of yacc.c  */
#line 2332 "parser.y"
    {
                    String *val = (yyvsp[(6) - (6)].str) ? NewString((yyvsp[(6) - (6)].str)) : NewString("1");
                    new_feature((yyvsp[(3) - (6)].id), val, (yyvsp[(4) - (6)].node), 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 91:

/* Line 1464 of yacc.c  */
#line 2338 "parser.y"
    {
                    String *val = Len((yyvsp[(5) - (8)].str)) ? (yyvsp[(5) - (8)].str) : 0;
                    new_feature((yyvsp[(3) - (8)].id), val, (yyvsp[(6) - (8)].node), 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 92:

/* Line 1464 of yacc.c  */
#line 2346 "parser.y"
    { (yyval.str) = (yyvsp[(1) - (1)].str); }
    break;

  case 93:

/* Line 1464 of yacc.c  */
#line 2347 "parser.y"
    { (yyval.str) = 0; }
    break;

  case 94:

/* Line 1464 of yacc.c  */
#line 2348 "parser.y"
    { (yyval.str) = (yyvsp[(3) - (5)].pl); }
    break;

  case 95:

/* Line 1464 of yacc.c  */
#line 2351 "parser.y"
    {
		  (yyval.node) = NewHash();
		  Setattr((yyval.node),"name",(yyvsp[(2) - (4)].id));
		  Setattr((yyval.node),"value",(yyvsp[(4) - (4)].str));
                }
    break;

  case 96:

/* Line 1464 of yacc.c  */
#line 2356 "parser.y"
    {
		  (yyval.node) = NewHash();
		  Setattr((yyval.node),"name",(yyvsp[(2) - (5)].id));
		  Setattr((yyval.node),"value",(yyvsp[(4) - (5)].str));
                  set_nextSibling((yyval.node),(yyvsp[(5) - (5)].node));
                }
    break;

  case 97:

/* Line 1464 of yacc.c  */
#line 2366 "parser.y"
    {
                 Parm *val;
		 String *name;
		 SwigType *t;
		 if (Namespaceprefix) name = NewStringf("%s::%s", Namespaceprefix, (yyvsp[(5) - (7)].decl).id);
		 else name = NewString((yyvsp[(5) - (7)].decl).id);
		 val = (yyvsp[(3) - (7)].pl);
		 if ((yyvsp[(5) - (7)].decl).parms) {
		   Setmeta(val,"parms",(yyvsp[(5) - (7)].decl).parms);
		 }
		 t = (yyvsp[(5) - (7)].decl).type;
		 if (!Len(t)) t = 0;
		 if (t) {
		   if ((yyvsp[(6) - (7)].dtype).qualifier) SwigType_push(t,(yyvsp[(6) - (7)].dtype).qualifier);
		   if (SwigType_isfunction(t)) {
		     SwigType *decl = SwigType_pop_function(t);
		     if (SwigType_ispointer(t)) {
		       String *nname = NewStringf("*%s",name);
		       Swig_feature_set(Swig_cparse_features(), nname, decl, "feature:varargs", val, 0);
		       Delete(nname);
		     } else {
		       Swig_feature_set(Swig_cparse_features(), name, decl, "feature:varargs", val, 0);
		     }
		     Delete(decl);
		   } else if (SwigType_ispointer(t)) {
		     String *nname = NewStringf("*%s",name);
		     Swig_feature_set(Swig_cparse_features(),nname,0,"feature:varargs",val, 0);
		     Delete(nname);
		   }
		 } else {
		   Swig_feature_set(Swig_cparse_features(),name,0,"feature:varargs",val, 0);
		 }
		 Delete(name);
		 (yyval.node) = 0;
              }
    break;

  case 98:

/* Line 1464 of yacc.c  */
#line 2402 "parser.y"
    { (yyval.pl) = (yyvsp[(1) - (1)].pl); }
    break;

  case 99:

/* Line 1464 of yacc.c  */
#line 2403 "parser.y"
    { 
		  int i;
		  int n;
		  Parm *p;
		  n = atoi(Char((yyvsp[(1) - (3)].dtype).val));
		  if (n <= 0) {
		    Swig_error(cparse_file, cparse_line,"Argument count in %%varargs must be positive.\n");
		    (yyval.pl) = 0;
		  } else {
		    String *name = Getattr((yyvsp[(3) - (3)].p), "name");
		    (yyval.pl) = Copy((yyvsp[(3) - (3)].p));
		    if (name)
		      Setattr((yyval.pl), "name", NewStringf("%s%d", name, n));
		    for (i = 1; i < n; i++) {
		      p = Copy((yyvsp[(3) - (3)].p));
		      name = Getattr(p, "name");
		      if (name)
		        Setattr(p, "name", NewStringf("%s%d", name, n-i));
		      set_nextSibling(p,(yyval.pl));
		      Delete((yyval.pl));
		      (yyval.pl) = p;
		    }
		  }
                }
    break;

  case 100:

/* Line 1464 of yacc.c  */
#line 2438 "parser.y"
    {
		   (yyval.node) = 0;
		   if ((yyvsp[(3) - (6)].tmap).method) {
		     String *code = 0;
		     (yyval.node) = new_node("typemap");
		     Setattr((yyval.node),"method",(yyvsp[(3) - (6)].tmap).method);
		     if ((yyvsp[(3) - (6)].tmap).kwargs) {
		       ParmList *kw = (yyvsp[(3) - (6)].tmap).kwargs;
                       code = remove_block(kw, (yyvsp[(6) - (6)].str));
		       Setattr((yyval.node),"kwargs", (yyvsp[(3) - (6)].tmap).kwargs);
		     }
		     code = code ? code : NewString((yyvsp[(6) - (6)].str));
		     Setattr((yyval.node),"code", code);
		     Delete(code);
		     appendChild((yyval.node),(yyvsp[(5) - (6)].p));
		   }
	       }
    break;

  case 101:

/* Line 1464 of yacc.c  */
#line 2455 "parser.y"
    {
		 (yyval.node) = 0;
		 if ((yyvsp[(3) - (6)].tmap).method) {
		   (yyval.node) = new_node("typemap");
		   Setattr((yyval.node),"method",(yyvsp[(3) - (6)].tmap).method);
		   appendChild((yyval.node),(yyvsp[(5) - (6)].p));
		 }
	       }
    break;

  case 102:

/* Line 1464 of yacc.c  */
#line 2463 "parser.y"
    {
		   (yyval.node) = 0;
		   if ((yyvsp[(3) - (8)].tmap).method) {
		     (yyval.node) = new_node("typemapcopy");
		     Setattr((yyval.node),"method",(yyvsp[(3) - (8)].tmap).method);
		     Setattr((yyval.node),"pattern", Getattr((yyvsp[(7) - (8)].p),"pattern"));
		     appendChild((yyval.node),(yyvsp[(5) - (8)].p));
		   }
	       }
    break;

  case 103:

/* Line 1464 of yacc.c  */
#line 2476 "parser.y"
    {
		 Hash *p;
		 String *name;
		 p = nextSibling((yyvsp[(1) - (1)].node));
		 if (p && (!Getattr(p,"value"))) {
 		   /* this is the deprecated two argument typemap form */
 		   Swig_warning(WARN_DEPRECATED_TYPEMAP_LANG,cparse_file, cparse_line,
				"Specifying the language name in %%typemap is deprecated - use #ifdef SWIG<LANG> instead.\n");
		   /* two argument typemap form */
		   name = Getattr((yyvsp[(1) - (1)].node),"name");
		   if (!name || (Strcmp(name,typemap_lang))) {
		     (yyval.tmap).method = 0;
		     (yyval.tmap).kwargs = 0;
		   } else {
		     (yyval.tmap).method = Getattr(p,"name");
		     (yyval.tmap).kwargs = nextSibling(p);
		   }
		 } else {
		   /* one-argument typemap-form */
		   (yyval.tmap).method = Getattr((yyvsp[(1) - (1)].node),"name");
		   (yyval.tmap).kwargs = p;
		 }
                }
    break;

  case 104:

/* Line 1464 of yacc.c  */
#line 2501 "parser.y"
    {
                 (yyval.p) = (yyvsp[(1) - (2)].p);
		 set_nextSibling((yyval.p),(yyvsp[(2) - (2)].p));
		}
    break;

  case 105:

/* Line 1464 of yacc.c  */
#line 2507 "parser.y"
    {
                 (yyval.p) = (yyvsp[(2) - (3)].p);
		 set_nextSibling((yyval.p),(yyvsp[(3) - (3)].p));
                }
    break;

  case 106:

/* Line 1464 of yacc.c  */
#line 2511 "parser.y"
    { (yyval.p) = 0;}
    break;

  case 107:

/* Line 1464 of yacc.c  */
#line 2514 "parser.y"
    {
                  Parm *parm;
		  SwigType_push((yyvsp[(1) - (2)].type),(yyvsp[(2) - (2)].decl).type);
		  (yyval.p) = new_node("typemapitem");
		  parm = NewParmWithoutFileLineInfo((yyvsp[(1) - (2)].type),(yyvsp[(2) - (2)].decl).id);
		  Setattr((yyval.p),"pattern",parm);
		  Setattr((yyval.p),"parms", (yyvsp[(2) - (2)].decl).parms);
		  Delete(parm);
		  /*		  $$ = NewParmWithoutFileLineInfo($1,$2.id);
				  Setattr($$,"parms",$2.parms); */
                }
    break;

  case 108:

/* Line 1464 of yacc.c  */
#line 2525 "parser.y"
    {
                  (yyval.p) = new_node("typemapitem");
		  Setattr((yyval.p),"pattern",(yyvsp[(2) - (3)].pl));
		  /*		  Setattr($$,"multitype",$2); */
               }
    break;

  case 109:

/* Line 1464 of yacc.c  */
#line 2530 "parser.y"
    {
		 (yyval.p) = new_node("typemapitem");
		 Setattr((yyval.p),"pattern", (yyvsp[(2) - (6)].pl));
		 /*                 Setattr($$,"multitype",$2); */
		 Setattr((yyval.p),"parms",(yyvsp[(5) - (6)].pl));
               }
    break;

  case 110:

/* Line 1464 of yacc.c  */
#line 2543 "parser.y"
    {
                   (yyval.node) = new_node("types");
		   Setattr((yyval.node),"parms",(yyvsp[(3) - (5)].pl));
                   if ((yyvsp[(5) - (5)].str))
		     Setattr((yyval.node),"convcode",NewString((yyvsp[(5) - (5)].str)));
               }
    break;

  case 111:

/* Line 1464 of yacc.c  */
#line 2555 "parser.y"
    {
                  Parm *p, *tp;
		  Node *n;
		  Node *outer_class = currentOuterClass;
		  Symtab *tscope = 0;
		  int     specialized = 0;
		  int     variadic = 0;

		  (yyval.node) = 0;

		  tscope = Swig_symbol_current();          /* Get the current scope */

		  /* If the class name is qualified, we need to create or lookup namespace entries */
		  if (!inclass) {
		    (yyvsp[(5) - (9)].str) = resolve_create_node_scope((yyvsp[(5) - (9)].str));
		  }
		  if (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0) {
		    outer_class	= nscope_inner;
		  }

		  /*
		    We use the new namespace entry 'nscope' only to
		    emit the template node. The template parameters are
		    resolved in the current 'tscope'.

		    This is closer to the C++ (typedef) behavior.
		  */
		  n = Swig_cparse_template_locate((yyvsp[(5) - (9)].str),(yyvsp[(7) - (9)].p),tscope);

		  /* Patch the argument types to respect namespaces */
		  p = (yyvsp[(7) - (9)].p);
		  while (p) {
		    SwigType *value = Getattr(p,"value");
		    if (!value) {
		      SwigType *ty = Getattr(p,"type");
		      if (ty) {
			SwigType *rty = 0;
			int reduce = template_reduce;
			if (reduce || !SwigType_ispointer(ty)) {
			  rty = Swig_symbol_typedef_reduce(ty,tscope);
			  if (!reduce) reduce = SwigType_ispointer(rty);
			}
			ty = reduce ? Swig_symbol_type_qualify(rty,tscope) : Swig_symbol_type_qualify(ty,tscope);
			Setattr(p,"type",ty);
			Delete(ty);
			Delete(rty);
		      }
		    } else {
		      value = Swig_symbol_type_qualify(value,tscope);
		      Setattr(p,"value",value);
		      Delete(value);
		    }

		    p = nextSibling(p);
		  }

		  /* Look for the template */
		  {
                    Node *nn = n;
                    Node *linklistend = 0;
                    Node *linkliststart = 0;
                    while (nn) {
                      Node *templnode = 0;
                      if (Strcmp(nodeType(nn),"template") == 0) {
                        int nnisclass = (Strcmp(Getattr(nn,"templatetype"),"class") == 0); /* if not a templated class it is a templated function */
                        Parm *tparms = Getattr(nn,"templateparms");
                        if (!tparms) {
                          specialized = 1;
                        } else if (Getattr(tparms,"variadic") && strncmp(Char(Getattr(tparms,"variadic")), "1", 1)==0) {
                          variadic = 1;
                        }
                        if (nnisclass && !variadic && !specialized && (ParmList_len((yyvsp[(7) - (9)].p)) > ParmList_len(tparms))) {
                          Swig_error(cparse_file, cparse_line, "Too many template parameters. Maximum of %d.\n", ParmList_len(tparms));
                        } else if (nnisclass && !specialized && ((ParmList_len((yyvsp[(7) - (9)].p)) < (ParmList_numrequired(tparms) - (variadic?1:0))))) { /* Variadic parameter is optional */
                          Swig_error(cparse_file, cparse_line, "Not enough template parameters specified. %d required.\n", (ParmList_numrequired(tparms)-(variadic?1:0)) );
                        } else if (!nnisclass && ((ParmList_len((yyvsp[(7) - (9)].p)) != ParmList_len(tparms)))) {
                          /* must be an overloaded templated method - ignore it as it is overloaded with a different number of template parameters */
                          nn = Getattr(nn,"sym:nextSibling"); /* repeat for overloaded templated functions */
                          continue;
                        } else {
			  String *tname = Copy((yyvsp[(5) - (9)].str));
                          int def_supplied = 0;
                          /* Expand the template */
			  Node *templ = Swig_symbol_clookup((yyvsp[(5) - (9)].str),0);
			  Parm *targs = templ ? Getattr(templ,"templateparms") : 0;

                          ParmList *temparms;
                          if (specialized) temparms = CopyParmList((yyvsp[(7) - (9)].p));
                          else temparms = CopyParmList(tparms);

                          /* Create typedef's and arguments */
                          p = (yyvsp[(7) - (9)].p);
                          tp = temparms;
                          if (!p && ParmList_len(p) != ParmList_len(temparms)) {
                            /* we have no template parameters supplied in %template for a template that has default args*/
                            p = tp;
                            def_supplied = 1;
                          }

                          while (p) {
                            String *value = Getattr(p,"value");
                            if (def_supplied) {
                              Setattr(p,"default","1");
                            }
                            if (value) {
                              Setattr(tp,"value",value);
                            } else {
                              SwigType *ty = Getattr(p,"type");
                              if (ty) {
                                Setattr(tp,"type",ty);
                              }
                              Delattr(tp,"value");
                            }
			    /* fix default arg values */
			    if (targs) {
			      Parm *pi = temparms;
			      Parm *ti = targs;
			      String *tv = Getattr(tp,"value");
			      if (!tv) tv = Getattr(tp,"type");
			      while(pi != tp && ti && pi) {
				String *name = Getattr(ti,"name");
				String *value = Getattr(pi,"value");
				if (!value) value = Getattr(pi,"type");
				Replaceid(tv, name, value);
				pi = nextSibling(pi);
				ti = nextSibling(ti);
			      }
			    }
                            p = nextSibling(p);
                            tp = nextSibling(tp);
                            if (!p && tp) {
                              p = tp;
                              def_supplied = 1;
                            } else if (p && !tp) { /* Variadic template - tp < p */
			      SWIG_WARN_NODE_BEGIN(nn);
                              Swig_warning(WARN_CPP11_VARIADIC_TEMPLATE,cparse_file, cparse_line,"Only the first variadic template argument is currently supported.\n");
			      SWIG_WARN_NODE_END(nn);
                              break;
                            }
                          }

                          templnode = copy_node(nn);
			  update_nested_classes(templnode); /* update classes nested within template */
                          /* We need to set the node name based on name used to instantiate */
                          Setattr(templnode,"name",tname);
			  Delete(tname);
                          if (!specialized) {
                            Delattr(templnode,"sym:typename");
                          } else {
                            Setattr(templnode,"sym:typename","1");
                          }
			  /* for now, nested %template is allowed only in the same scope as the template declaration */
                          if ((yyvsp[(3) - (9)].id) && !(nnisclass && ((outer_class && (outer_class != Getattr(nn, "nested:outer")))
			    ||(extendmode && current_class && (current_class != Getattr(nn, "nested:outer")))))) {
			    /*
			       Comment this out for 1.3.28. We need to
			       re-enable it later but first we need to
			       move %ignore from using %rename to use
			       %feature(ignore).

			       String *symname = Swig_name_make(templnode,0,$3,0,0);
			    */
			    String *symname = NewString((yyvsp[(3) - (9)].id));
                            Swig_cparse_template_expand(templnode,symname,temparms,tscope);
                            Setattr(templnode,"sym:name",symname);
                          } else {
                            static int cnt = 0;
                            String *nname = NewStringf("__dummy_%d__", cnt++);
                            Swig_cparse_template_expand(templnode,nname,temparms,tscope);
                            Setattr(templnode,"sym:name",nname);
			    Delete(nname);
                            Setattr(templnode,"feature:onlychildren", "typemap,typemapitem,typemapcopy,typedef,types,fragment");
			    if ((yyvsp[(3) - (9)].id)) {
			      Swig_warning(WARN_PARSE_NESTED_TEMPLATE, cparse_file, cparse_line, "Named nested template instantiations not supported. Processing as if no name was given to %%template().\n");
			    }
                          }
                          Delattr(templnode,"templatetype");
                          Setattr(templnode,"template",nn);
                          Setfile(templnode,cparse_file);
                          Setline(templnode,cparse_line);
                          Delete(temparms);
			  if (outer_class && nnisclass) {
			    SetFlag(templnode, "nested");
			    Setattr(templnode, "nested:outer", outer_class);
			  }
                          add_symbols_copy(templnode);

                          if (Strcmp(nodeType(templnode),"class") == 0) {

                            /* Identify pure abstract methods */
                            Setattr(templnode,"abstracts", pure_abstracts(firstChild(templnode)));

                            /* Set up inheritance in symbol table */
                            {
                              Symtab  *csyms;
                              List *baselist = Getattr(templnode,"baselist");
                              csyms = Swig_symbol_current();
                              Swig_symbol_setscope(Getattr(templnode,"symtab"));
                              if (baselist) {
                                List *bases = Swig_make_inherit_list(Getattr(templnode,"name"),baselist, Namespaceprefix);
                                if (bases) {
                                  Iterator s;
                                  for (s = First(bases); s.item; s = Next(s)) {
                                    Symtab *st = Getattr(s.item,"symtab");
                                    if (st) {
				      Setfile(st,Getfile(s.item));
				      Setline(st,Getline(s.item));
                                      Swig_symbol_inherit(st);
                                    }
                                  }
				  Delete(bases);
                                }
                              }
                              Swig_symbol_setscope(csyms);
                            }

                            /* Merge in %extend methods for this class.
			       This only merges methods within %extend for a template specialized class such as
			         template<typename T> class K {}; %extend K<int> { ... }
			       The copy_node() call above has already added in the generic %extend methods such as
			         template<typename T> class K {}; %extend K { ... } */

			    /* !!! This may be broken.  We may have to add the
			       %extend methods at the beginning of the class */
                            {
                              String *stmp = 0;
                              String *clsname;
                              Node *am;
                              if (Namespaceprefix) {
                                clsname = stmp = NewStringf("%s::%s", Namespaceprefix, Getattr(templnode,"name"));
                              } else {
                                clsname = Getattr(templnode,"name");
                              }
                              am = Getattr(Swig_extend_hash(),clsname);
                              if (am) {
                                Symtab *st = Swig_symbol_current();
                                Swig_symbol_setscope(Getattr(templnode,"symtab"));
                                /*			    Printf(stdout,"%s: %s %p %p\n", Getattr(templnode,"name"), clsname, Swig_symbol_current(), Getattr(templnode,"symtab")); */
                                Swig_extend_merge(templnode,am);
                                Swig_symbol_setscope(st);
				Swig_extend_append_previous(templnode,am);
                                Delattr(Swig_extend_hash(),clsname);
                              }
			      if (stmp) Delete(stmp);
                            }

                            /* Add to classes hash */
			    if (!classes)
			      classes = NewHash();

			    if (Namespaceprefix) {
			      String *temp = NewStringf("%s::%s", Namespaceprefix, Getattr(templnode,"name"));
			      Setattr(classes,temp,templnode);
			      Delete(temp);
			    } else {
			      String *qs = Swig_symbol_qualifiedscopename(templnode);
			      Setattr(classes, qs,templnode);
			      Delete(qs);
			    }
                          }
                        }

                        /* all the overloaded templated functions are added into a linked list */
                        if (!linkliststart)
                          linkliststart = templnode;
                        if (nscope_inner) {
                          /* non-global namespace */
                          if (templnode) {
                            appendChild(nscope_inner,templnode);
			    Delete(templnode);
                            if (nscope) (yyval.node) = nscope;
                          }
                        } else {
                          /* global namespace */
                          if (!linklistend) {
                            (yyval.node) = templnode;
                          } else {
                            set_nextSibling(linklistend,templnode);
			    Delete(templnode);
                          }
                          linklistend = templnode;
                        }
                      }
                      nn = Getattr(nn,"sym:nextSibling"); /* repeat for overloaded templated functions. If a templated class there will never be a sibling. */
                    }
                    update_defaultargs(linkliststart);
		  }
	          Swig_symbol_setscope(tscope);
		  Delete(Namespaceprefix);
		  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
                }
    break;

  case 112:

/* Line 1464 of yacc.c  */
#line 2853 "parser.y"
    {
		  Swig_warning(0,cparse_file, cparse_line,"%s\n", (yyvsp[(2) - (2)].str));
		  (yyval.node) = 0;
               }
    break;

  case 113:

/* Line 1464 of yacc.c  */
#line 2863 "parser.y"
    {
                    (yyval.node) = (yyvsp[(1) - (1)].node); 
                    if ((yyval.node)) {
   		      add_symbols((yyval.node));
                      default_arguments((yyval.node));
   	            }
                }
    break;

  case 114:

/* Line 1464 of yacc.c  */
#line 2870 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 115:

/* Line 1464 of yacc.c  */
#line 2871 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 116:

/* Line 1464 of yacc.c  */
#line 2875 "parser.y"
    {
		  if (Strcmp((yyvsp[(2) - (3)].str),"C") == 0) {
		    cparse_externc = 1;
		  }
		}
    break;

  case 117:

/* Line 1464 of yacc.c  */
#line 2879 "parser.y"
    {
		  cparse_externc = 0;
		  if (Strcmp((yyvsp[(2) - (6)].str),"C") == 0) {
		    Node *n = firstChild((yyvsp[(5) - (6)].node));
		    (yyval.node) = new_node("extern");
		    Setattr((yyval.node),"name",(yyvsp[(2) - (6)].str));
		    appendChild((yyval.node),n);
		    while (n) {
		      SwigType *decl = Getattr(n,"decl");
		      if (SwigType_isfunction(decl) && !Equal(Getattr(n, "storage"), "typedef")) {
			Setattr(n,"storage","externc");
		      }
		      n = nextSibling(n);
		    }
		  } else {
		     Swig_warning(WARN_PARSE_UNDEFINED_EXTERN,cparse_file, cparse_line,"Unrecognized extern type \"%s\".\n", (yyvsp[(2) - (6)].str));
		    (yyval.node) = new_node("extern");
		    Setattr((yyval.node),"name",(yyvsp[(2) - (6)].str));
		    appendChild((yyval.node),firstChild((yyvsp[(5) - (6)].node)));
		  }
                }
    break;

  case 118:

/* Line 1464 of yacc.c  */
#line 2900 "parser.y"
    {
		  (yyval.node) = (yyvsp[(1) - (1)].node);
		  SWIG_WARN_NODE_BEGIN((yyval.node));
		  Swig_warning(WARN_CPP11_LAMBDA, cparse_file, cparse_line, "Lambda expressions and closures are not fully supported yet.\n");
		  SWIG_WARN_NODE_END((yyval.node));
		}
    break;

  case 119:

/* Line 1464 of yacc.c  */
#line 2906 "parser.y"
    {
		  /* Convert using statement to a typedef statement */
		  (yyval.node) = new_node("cdecl");
		  Setattr((yyval.node),"type",(yyvsp[(4) - (6)].type));
		  Setattr((yyval.node),"storage","typedef");
		  Setattr((yyval.node),"name",(yyvsp[(2) - (6)].str));
		  Setattr((yyval.node),"decl",(yyvsp[(5) - (6)].decl).type);
		  SetFlag((yyval.node),"typealias");
		  add_symbols((yyval.node));
		}
    break;

  case 120:

/* Line 1464 of yacc.c  */
#line 2916 "parser.y"
    {
		  /* Convert alias template to a "template" typedef statement */
		  (yyval.node) = new_node("template");
		  Setattr((yyval.node),"type",(yyvsp[(8) - (10)].type));
		  Setattr((yyval.node),"storage","typedef");
		  Setattr((yyval.node),"name",(yyvsp[(6) - (10)].str));
		  Setattr((yyval.node),"decl",(yyvsp[(9) - (10)].decl).type);
		  Setattr((yyval.node),"templateparms",(yyvsp[(3) - (10)].tparms));
		  Setattr((yyval.node),"templatetype","cdecl");
		  SetFlag((yyval.node),"aliastemplate");
		  add_symbols((yyval.node));
		}
    break;

  case 121:

/* Line 1464 of yacc.c  */
#line 2934 "parser.y"
    {
              (yyval.node) = new_node("cdecl");
	      if ((yyvsp[(4) - (5)].dtype).qualifier) SwigType_push((yyvsp[(3) - (5)].decl).type,(yyvsp[(4) - (5)].dtype).qualifier);
	      Setattr((yyval.node),"type",(yyvsp[(2) - (5)].type));
	      Setattr((yyval.node),"storage",(yyvsp[(1) - (5)].id));
	      Setattr((yyval.node),"name",(yyvsp[(3) - (5)].decl).id);
	      Setattr((yyval.node),"decl",(yyvsp[(3) - (5)].decl).type);
	      Setattr((yyval.node),"parms",(yyvsp[(3) - (5)].decl).parms);
	      Setattr((yyval.node),"value",(yyvsp[(4) - (5)].dtype).val);
	      Setattr((yyval.node),"throws",(yyvsp[(4) - (5)].dtype).throws);
	      Setattr((yyval.node),"throw",(yyvsp[(4) - (5)].dtype).throwf);
	      Setattr((yyval.node),"noexcept",(yyvsp[(4) - (5)].dtype).nexcept);
	      if ((yyvsp[(4) - (5)].dtype).val && (yyvsp[(4) - (5)].dtype).type) {
		/* store initializer type as it might be different to the declared type */
		SwigType *valuetype = NewSwigType((yyvsp[(4) - (5)].dtype).type);
		if (Len(valuetype) > 0)
		  Setattr((yyval.node),"valuetype",valuetype);
		else
		  Delete(valuetype);
	      }
	      if (!(yyvsp[(5) - (5)].node)) {
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
	      } else {
		Node *n = (yyvsp[(5) - (5)].node);
		/* Inherit attributes */
		while (n) {
		  String *type = Copy((yyvsp[(2) - (5)].type));
		  Setattr(n,"type",type);
		  Setattr(n,"storage",(yyvsp[(1) - (5)].id));
		  n = nextSibling(n);
		  Delete(type);
		}
	      }
	      if ((yyvsp[(4) - (5)].dtype).bitfield) {
		Setattr((yyval.node),"bitfield", (yyvsp[(4) - (5)].dtype).bitfield);
	      }

	      /* Look for "::" declarations (ignored) */
	      if (Strstr((yyvsp[(3) - (5)].decl).id,"::")) {
                /* This is a special case. If the scope name of the declaration exactly
                   matches that of the declaration, then we will allow it. Otherwise, delete. */
                String *p = Swig_scopename_prefix((yyvsp[(3) - (5)].decl).id);
		if (p) {
		  if ((Namespaceprefix && Strcmp(p, Namespaceprefix) == 0) ||
		      (Classprefix && Strcmp(p, Classprefix) == 0)) {
		    String *lstr = Swig_scopename_last((yyvsp[(3) - (5)].decl).id);
		    Setattr((yyval.node),"name",lstr);
		    Delete(lstr);
		    set_nextSibling((yyval.node),(yyvsp[(5) - (5)].node));
		  } else {
		    Delete((yyval.node));
		    (yyval.node) = (yyvsp[(5) - (5)].node);
		  }
		  Delete(p);
		} else {
		  Delete((yyval.node));
		  (yyval.node) = (yyvsp[(5) - (5)].node);
		}
	      } else {
		set_nextSibling((yyval.node),(yyvsp[(5) - (5)].node));
	      }
           }
    break;

  case 122:

/* Line 1464 of yacc.c  */
#line 3002 "parser.y"
    {
              (yyval.node) = new_node("cdecl");
	      if ((yyvsp[(6) - (7)].dtype).qualifier) SwigType_push((yyvsp[(3) - (7)].decl).type,(yyvsp[(6) - (7)].dtype).qualifier);
	      Setattr((yyval.node),"type",(yyvsp[(5) - (7)].node));
	      Setattr((yyval.node),"storage",(yyvsp[(1) - (7)].id));
	      Setattr((yyval.node),"name",(yyvsp[(3) - (7)].decl).id);
	      Setattr((yyval.node),"decl",(yyvsp[(3) - (7)].decl).type);
	      Setattr((yyval.node),"parms",(yyvsp[(3) - (7)].decl).parms);
	      Setattr((yyval.node),"value",(yyvsp[(6) - (7)].dtype).val);
	      Setattr((yyval.node),"throws",(yyvsp[(6) - (7)].dtype).throws);
	      Setattr((yyval.node),"throw",(yyvsp[(6) - (7)].dtype).throwf);
	      Setattr((yyval.node),"noexcept",(yyvsp[(6) - (7)].dtype).nexcept);
	      if (!(yyvsp[(7) - (7)].node)) {
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
	      } else {
		Node *n = (yyvsp[(7) - (7)].node);
		while (n) {
		  String *type = Copy((yyvsp[(5) - (7)].node));
		  Setattr(n,"type",type);
		  Setattr(n,"storage",(yyvsp[(1) - (7)].id));
		  n = nextSibling(n);
		  Delete(type);
		}
	      }
	      if ((yyvsp[(6) - (7)].dtype).bitfield) {
		Setattr((yyval.node),"bitfield", (yyvsp[(6) - (7)].dtype).bitfield);
	      }

	      if (Strstr((yyvsp[(3) - (7)].decl).id,"::")) {
                String *p = Swig_scopename_prefix((yyvsp[(3) - (7)].decl).id);
		if (p) {
		  if ((Namespaceprefix && Strcmp(p, Namespaceprefix) == 0) ||
		      (Classprefix && Strcmp(p, Classprefix) == 0)) {
		    String *lstr = Swig_scopename_last((yyvsp[(3) - (7)].decl).id);
		    Setattr((yyval.node),"name",lstr);
		    Delete(lstr);
		    set_nextSibling((yyval.node),(yyvsp[(7) - (7)].node));
		  } else {
		    Delete((yyval.node));
		    (yyval.node) = (yyvsp[(7) - (7)].node);
		  }
		  Delete(p);
		} else {
		  Delete((yyval.node));
		  (yyval.node) = (yyvsp[(7) - (7)].node);
		}
	      } else {
		set_nextSibling((yyval.node),(yyvsp[(7) - (7)].node));
	      }
           }
    break;

  case 123:

/* Line 1464 of yacc.c  */
#line 3060 "parser.y"
    { 
                   (yyval.node) = 0;
                   Clear(scanner_ccode); 
               }
    break;

  case 124:

/* Line 1464 of yacc.c  */
#line 3064 "parser.y"
    {
		 (yyval.node) = new_node("cdecl");
		 if ((yyvsp[(3) - (4)].dtype).qualifier) SwigType_push((yyvsp[(2) - (4)].decl).type,(yyvsp[(3) - (4)].dtype).qualifier);
		 Setattr((yyval.node),"name",(yyvsp[(2) - (4)].decl).id);
		 Setattr((yyval.node),"decl",(yyvsp[(2) - (4)].decl).type);
		 Setattr((yyval.node),"parms",(yyvsp[(2) - (4)].decl).parms);
		 Setattr((yyval.node),"value",(yyvsp[(3) - (4)].dtype).val);
		 Setattr((yyval.node),"throws",(yyvsp[(3) - (4)].dtype).throws);
		 Setattr((yyval.node),"throw",(yyvsp[(3) - (4)].dtype).throwf);
		 Setattr((yyval.node),"noexcept",(yyvsp[(3) - (4)].dtype).nexcept);
		 if ((yyvsp[(3) - (4)].dtype).bitfield) {
		   Setattr((yyval.node),"bitfield", (yyvsp[(3) - (4)].dtype).bitfield);
		 }
		 if (!(yyvsp[(4) - (4)].node)) {
		   if (Len(scanner_ccode)) {
		     String *code = Copy(scanner_ccode);
		     Setattr((yyval.node),"code",code);
		     Delete(code);
		   }
		 } else {
		   set_nextSibling((yyval.node),(yyvsp[(4) - (4)].node));
		 }
	       }
    break;

  case 125:

/* Line 1464 of yacc.c  */
#line 3087 "parser.y"
    { 
                   skip_balanced('{','}');
                   (yyval.node) = 0;
               }
    break;

  case 126:

/* Line 1464 of yacc.c  */
#line 3091 "parser.y"
    {
		   (yyval.node) = 0;
		   if (yychar == RPAREN) {
		       Swig_error(cparse_file, cparse_line, "Unexpected ')'.\n");
		   } else {
		       Swig_error(cparse_file, cparse_line, "Syntax error - possibly a missing semicolon.\n");
		   }
		   exit(1);
               }
    break;

  case 127:

/* Line 1464 of yacc.c  */
#line 3102 "parser.y"
    { 
                   (yyval.dtype) = (yyvsp[(1) - (1)].dtype); 
                   (yyval.dtype).qualifier = 0;
		   (yyval.dtype).throws = 0;
		   (yyval.dtype).throwf = 0;
		   (yyval.dtype).nexcept = 0;
              }
    break;

  case 128:

/* Line 1464 of yacc.c  */
#line 3109 "parser.y"
    { 
                   (yyval.dtype) = (yyvsp[(2) - (2)].dtype); 
		   (yyval.dtype).qualifier = (yyvsp[(1) - (2)].str);
		   (yyval.dtype).throws = 0;
		   (yyval.dtype).throwf = 0;
		   (yyval.dtype).nexcept = 0;
	      }
    break;

  case 129:

/* Line 1464 of yacc.c  */
#line 3116 "parser.y"
    { 
		   (yyval.dtype) = (yyvsp[(2) - (2)].dtype); 
                   (yyval.dtype).qualifier = 0;
		   (yyval.dtype).throws = (yyvsp[(1) - (2)].dtype).throws;
		   (yyval.dtype).throwf = (yyvsp[(1) - (2)].dtype).throwf;
		   (yyval.dtype).nexcept = (yyvsp[(1) - (2)].dtype).nexcept;
              }
    break;

  case 130:

/* Line 1464 of yacc.c  */
#line 3123 "parser.y"
    { 
                   (yyval.dtype) = (yyvsp[(3) - (3)].dtype); 
                   (yyval.dtype).qualifier = (yyvsp[(1) - (3)].str);
		   (yyval.dtype).throws = (yyvsp[(2) - (3)].dtype).throws;
		   (yyval.dtype).throwf = (yyvsp[(2) - (3)].dtype).throwf;
		   (yyval.dtype).nexcept = (yyvsp[(2) - (3)].dtype).nexcept;
              }
    break;

  case 131:

/* Line 1464 of yacc.c  */
#line 3132 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].type); }
    break;

  case 132:

/* Line 1464 of yacc.c  */
#line 3133 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].type); }
    break;

  case 133:

/* Line 1464 of yacc.c  */
#line 3134 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].type); }
    break;

  case 134:

/* Line 1464 of yacc.c  */
#line 3138 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].type); }
    break;

  case 135:

/* Line 1464 of yacc.c  */
#line 3139 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].str); }
    break;

  case 136:

/* Line 1464 of yacc.c  */
#line 3140 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].type); }
    break;

  case 137:

/* Line 1464 of yacc.c  */
#line 3151 "parser.y"
    {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[(3) - (11)].str));
		  add_symbols((yyval.node));
	        }
    break;

  case 138:

/* Line 1464 of yacc.c  */
#line 3156 "parser.y"
    {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[(3) - (13)].str));
		  add_symbols((yyval.node));
		}
    break;

  case 139:

/* Line 1464 of yacc.c  */
#line 3161 "parser.y"
    {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[(3) - (7)].str));
		  add_symbols((yyval.node));
		}
    break;

  case 140:

/* Line 1464 of yacc.c  */
#line 3168 "parser.y"
    {
		  skip_balanced('[',']');
		  (yyval.node) = 0;
	        }
    break;

  case 141:

/* Line 1464 of yacc.c  */
#line 3174 "parser.y"
    {
		  skip_balanced('{','}');
		  (yyval.node) = 0;
		}
    break;

  case 142:

/* Line 1464 of yacc.c  */
#line 3179 "parser.y"
    {
		  (yyval.pl) = 0;
		}
    break;

  case 143:

/* Line 1464 of yacc.c  */
#line 3182 "parser.y"
    {
		  skip_balanced('(',')');
		}
    break;

  case 144:

/* Line 1464 of yacc.c  */
#line 3184 "parser.y"
    {
		  (yyval.pl) = 0;
		}
    break;

  case 145:

/* Line 1464 of yacc.c  */
#line 3195 "parser.y"
    {
		   (yyval.node) = (char *)"enum";
	      }
    break;

  case 146:

/* Line 1464 of yacc.c  */
#line 3198 "parser.y"
    {
		   (yyval.node) = (char *)"enum class";
	      }
    break;

  case 147:

/* Line 1464 of yacc.c  */
#line 3201 "parser.y"
    {
		   (yyval.node) = (char *)"enum struct";
	      }
    break;

  case 148:

/* Line 1464 of yacc.c  */
#line 3210 "parser.y"
    {
                   (yyval.node) = (yyvsp[(2) - (2)].type);
              }
    break;

  case 149:

/* Line 1464 of yacc.c  */
#line 3213 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 150:

/* Line 1464 of yacc.c  */
#line 3220 "parser.y"
    {
		   SwigType *ty = 0;
		   int scopedenum = (yyvsp[(3) - (5)].id) && !Equal((yyvsp[(2) - (5)].node), "enum");
		   (yyval.node) = new_node("enumforward");
		   ty = NewStringf("enum %s", (yyvsp[(3) - (5)].id));
		   Setattr((yyval.node),"enumkey",(yyvsp[(2) - (5)].node));
		   if (scopedenum)
		     SetFlag((yyval.node), "scopedenum");
		   Setattr((yyval.node),"name",(yyvsp[(3) - (5)].id));
		   Setattr((yyval.node),"inherit",(yyvsp[(4) - (5)].node));
		   Setattr((yyval.node),"type",ty);
		   Setattr((yyval.node),"sym:weak", "1");
		   add_symbols((yyval.node));
	      }
    break;

  case 151:

/* Line 1464 of yacc.c  */
#line 3242 "parser.y"
    {
		  SwigType *ty = 0;
		  int scopedenum = (yyvsp[(3) - (8)].id) && !Equal((yyvsp[(2) - (8)].node), "enum");
                  (yyval.node) = new_node("enum");
		  ty = NewStringf("enum %s", (yyvsp[(3) - (8)].id));
		  Setattr((yyval.node),"enumkey",(yyvsp[(2) - (8)].node));
		  if (scopedenum)
		    SetFlag((yyval.node), "scopedenum");
		  Setattr((yyval.node),"name",(yyvsp[(3) - (8)].id));
		  Setattr((yyval.node),"inherit",(yyvsp[(4) - (8)].node));
		  Setattr((yyval.node),"type",ty);
		  appendChild((yyval.node),(yyvsp[(6) - (8)].node));
		  add_symbols((yyval.node));      /* Add to tag space */

		  if (scopedenum) {
		    Swig_symbol_newscope();
		    Swig_symbol_setscopename((yyvsp[(3) - (8)].id));
		    Delete(Namespaceprefix);
		    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		  }

		  add_symbols((yyvsp[(6) - (8)].node));      /* Add enum values to appropriate enum or enum class scope */

		  if (scopedenum) {
		    Setattr((yyval.node),"symtab", Swig_symbol_popscope());
		    Delete(Namespaceprefix);
		    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		  }
               }
    break;

  case 152:

/* Line 1464 of yacc.c  */
#line 3271 "parser.y"
    {
		 Node *n;
		 SwigType *ty = 0;
		 String   *unnamed = 0;
		 int       unnamedinstance = 0;
		 int scopedenum = (yyvsp[(3) - (10)].id) && !Equal((yyvsp[(2) - (10)].node), "enum");

		 (yyval.node) = new_node("enum");
		 Setattr((yyval.node),"enumkey",(yyvsp[(2) - (10)].node));
		 if (scopedenum)
		   SetFlag((yyval.node), "scopedenum");
		 Setattr((yyval.node),"inherit",(yyvsp[(4) - (10)].node));
		 if ((yyvsp[(3) - (10)].id)) {
		   Setattr((yyval.node),"name",(yyvsp[(3) - (10)].id));
		   ty = NewStringf("enum %s", (yyvsp[(3) - (10)].id));
		 } else if ((yyvsp[(8) - (10)].decl).id) {
		   unnamed = make_unnamed();
		   ty = NewStringf("enum %s", unnamed);
		   Setattr((yyval.node),"unnamed",unnamed);
                   /* name is not set for unnamed enum instances, e.g. enum { foo } Instance; */
		   if ((yyvsp[(1) - (10)].id) && Cmp((yyvsp[(1) - (10)].id),"typedef") == 0) {
		     Setattr((yyval.node),"name",(yyvsp[(8) - (10)].decl).id);
                   } else {
                     unnamedinstance = 1;
                   }
		   Setattr((yyval.node),"storage",(yyvsp[(1) - (10)].id));
		 }
		 if ((yyvsp[(8) - (10)].decl).id && Cmp((yyvsp[(1) - (10)].id),"typedef") == 0) {
		   Setattr((yyval.node),"tdname",(yyvsp[(8) - (10)].decl).id);
                   Setattr((yyval.node),"allows_typedef","1");
                 }
		 appendChild((yyval.node),(yyvsp[(6) - (10)].node));
		 n = new_node("cdecl");
		 Setattr(n,"type",ty);
		 Setattr(n,"name",(yyvsp[(8) - (10)].decl).id);
		 Setattr(n,"storage",(yyvsp[(1) - (10)].id));
		 Setattr(n,"decl",(yyvsp[(8) - (10)].decl).type);
		 Setattr(n,"parms",(yyvsp[(8) - (10)].decl).parms);
		 Setattr(n,"unnamed",unnamed);

                 if (unnamedinstance) {
		   SwigType *cty = NewString("enum ");
		   Setattr((yyval.node),"type",cty);
		   SetFlag((yyval.node),"unnamedinstance");
		   SetFlag(n,"unnamedinstance");
		   Delete(cty);
                 }
		 if ((yyvsp[(10) - (10)].node)) {
		   Node *p = (yyvsp[(10) - (10)].node);
		   set_nextSibling(n,p);
		   while (p) {
		     SwigType *cty = Copy(ty);
		     Setattr(p,"type",cty);
		     Setattr(p,"unnamed",unnamed);
		     Setattr(p,"storage",(yyvsp[(1) - (10)].id));
		     Delete(cty);
		     p = nextSibling(p);
		   }
		 } else {
		   if (Len(scanner_ccode)) {
		     String *code = Copy(scanner_ccode);
		     Setattr(n,"code",code);
		     Delete(code);
		   }
		 }

                 /* Ensure that typedef enum ABC {foo} XYZ; uses XYZ for sym:name, like structs.
                  * Note that class_rename/yyrename are bit of a mess so used this simple approach to change the name. */
                 if ((yyvsp[(8) - (10)].decl).id && (yyvsp[(3) - (10)].id) && Cmp((yyvsp[(1) - (10)].id),"typedef") == 0) {
		   String *name = NewString((yyvsp[(8) - (10)].decl).id);
                   Setattr((yyval.node), "parser:makename", name);
		   Delete(name);
                 }

		 add_symbols((yyval.node));       /* Add enum to tag space */
		 set_nextSibling((yyval.node),n);
		 Delete(n);

		 if (scopedenum) {
		   Swig_symbol_newscope();
		   Swig_symbol_setscopename((yyvsp[(3) - (10)].id));
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 }

		 add_symbols((yyvsp[(6) - (10)].node));      /* Add enum values to appropriate enum or enum class scope */

		 if (scopedenum) {
		   Setattr((yyval.node),"symtab", Swig_symbol_popscope());
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 }

	         add_symbols(n);
		 Delete(unnamed);
	       }
    break;

  case 153:

/* Line 1464 of yacc.c  */
#line 3369 "parser.y"
    {
                   /* This is a sick hack.  If the ctor_end has parameters,
                      and the parms parameter only has 1 parameter, this
                      could be a declaration of the form:

                         type (id)(parms)

			 Otherwise it's an error. */
                    int err = 0;
                    (yyval.node) = 0;

		    if ((ParmList_len((yyvsp[(4) - (6)].pl)) == 1) && (!Swig_scopename_check((yyvsp[(2) - (6)].type)))) {
		      SwigType *ty = Getattr((yyvsp[(4) - (6)].pl),"type");
		      String *name = Getattr((yyvsp[(4) - (6)].pl),"name");
		      err = 1;
		      if (!name) {
			(yyval.node) = new_node("cdecl");
			Setattr((yyval.node),"type",(yyvsp[(2) - (6)].type));
			Setattr((yyval.node),"storage",(yyvsp[(1) - (6)].id));
			Setattr((yyval.node),"name",ty);

			if ((yyvsp[(6) - (6)].decl).have_parms) {
			  SwigType *decl = NewStringEmpty();
			  SwigType_add_function(decl,(yyvsp[(6) - (6)].decl).parms);
			  Setattr((yyval.node),"decl",decl);
			  Setattr((yyval.node),"parms",(yyvsp[(6) - (6)].decl).parms);
			  if (Len(scanner_ccode)) {
			    String *code = Copy(scanner_ccode);
			    Setattr((yyval.node),"code",code);
			    Delete(code);
			  }
			}
			if ((yyvsp[(6) - (6)].decl).defarg) {
			  Setattr((yyval.node),"value",(yyvsp[(6) - (6)].decl).defarg);
			}
			Setattr((yyval.node),"throws",(yyvsp[(6) - (6)].decl).throws);
			Setattr((yyval.node),"throw",(yyvsp[(6) - (6)].decl).throwf);
			Setattr((yyval.node),"noexcept",(yyvsp[(6) - (6)].decl).nexcept);
			err = 0;
		      }
		    }
		    if (err) {
		      Swig_error(cparse_file,cparse_line,"Syntax error in input(2).\n");
		      exit(1);
		    }
                }
    break;

  case 154:

/* Line 1464 of yacc.c  */
#line 3421 "parser.y"
    {  (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 155:

/* Line 1464 of yacc.c  */
#line 3422 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 156:

/* Line 1464 of yacc.c  */
#line 3423 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 157:

/* Line 1464 of yacc.c  */
#line 3424 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 158:

/* Line 1464 of yacc.c  */
#line 3425 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 159:

/* Line 1464 of yacc.c  */
#line 3426 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 160:

/* Line 1464 of yacc.c  */
#line 3431 "parser.y"
    {
                   String *prefix;
                   List *bases = 0;
		   Node *scope = 0;
		   String *code;
		   (yyval.node) = new_node("class");
		   Setline((yyval.node),cparse_start_line);
		   Setattr((yyval.node),"kind",(yyvsp[(2) - (5)].id));
		   if ((yyvsp[(4) - (5)].bases)) {
		     Setattr((yyval.node),"baselist", Getattr((yyvsp[(4) - (5)].bases),"public"));
		     Setattr((yyval.node),"protectedbaselist", Getattr((yyvsp[(4) - (5)].bases),"protected"));
		     Setattr((yyval.node),"privatebaselist", Getattr((yyvsp[(4) - (5)].bases),"private"));
		   }
		   Setattr((yyval.node),"allows_typedef","1");

		   /* preserve the current scope */
		   Setattr((yyval.node),"prev_symtab",Swig_symbol_current());
		  
		   /* If the class name is qualified.  We need to create or lookup namespace/scope entries */
		   scope = resolve_create_node_scope((yyvsp[(3) - (5)].str));
		   /* save nscope_inner to the class - it may be overwritten in nested classes*/
		   Setattr((yyval.node), "nested:innerscope", nscope_inner);
		   Setattr((yyval.node), "nested:nscope", nscope);
		   Setfile(scope,cparse_file);
		   Setline(scope,cparse_line);
		   (yyvsp[(3) - (5)].str) = scope;
		   Setattr((yyval.node),"name",(yyvsp[(3) - (5)].str));

		   if (currentOuterClass) {
		     SetFlag((yyval.node), "nested");
		     Setattr((yyval.node), "nested:outer", currentOuterClass);
		     set_access_mode((yyval.node));
		   }
		   Swig_features_get(Swig_cparse_features(), Namespaceprefix, Getattr((yyval.node), "name"), 0, (yyval.node));
		   /* save yyrename to the class attribute, to be used later in add_symbols()*/
		   Setattr((yyval.node), "class_rename", make_name((yyval.node), (yyvsp[(3) - (5)].str), 0));
		   Setattr((yyval.node), "Classprefix", (yyvsp[(3) - (5)].str));
		   Classprefix = NewString((yyvsp[(3) - (5)].str));
		   /* Deal with inheritance  */
		   if ((yyvsp[(4) - (5)].bases))
		     bases = Swig_make_inherit_list((yyvsp[(3) - (5)].str),Getattr((yyvsp[(4) - (5)].bases),"public"),Namespaceprefix);
		   prefix = SwigType_istemplate_templateprefix((yyvsp[(3) - (5)].str));
		   if (prefix) {
		     String *fbase, *tbase;
		     if (Namespaceprefix) {
		       fbase = NewStringf("%s::%s", Namespaceprefix,(yyvsp[(3) - (5)].str));
		       tbase = NewStringf("%s::%s", Namespaceprefix, prefix);
		     } else {
		       fbase = Copy((yyvsp[(3) - (5)].str));
		       tbase = Copy(prefix);
		     }
		     Swig_name_inherit(tbase,fbase);
		     Delete(fbase);
		     Delete(tbase);
		   }
                   if (strcmp((yyvsp[(2) - (5)].id),"class") == 0) {
		     cplus_mode = CPLUS_PRIVATE;
		   } else {
		     cplus_mode = CPLUS_PUBLIC;
		   }
		   if (!cparse_cplusplus) {
		     set_scope_to_global();
		   }
		   Swig_symbol_newscope();
		   Swig_symbol_setscopename((yyvsp[(3) - (5)].str));
		   Swig_inherit_base_symbols(bases);
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   cparse_start_line = cparse_line;

		   /* If there are active template parameters, we need to make sure they are
                      placed in the class symbol table so we can catch shadows */

		   if (template_parameters) {
		     Parm *tp = template_parameters;
		     while(tp) {
		       String *tpname = Copy(Getattr(tp,"name"));
		       Node *tn = new_node("templateparm");
		       Setattr(tn,"name",tpname);
		       Swig_symbol_cadd(tpname,tn);
		       tp = nextSibling(tp);
		       Delete(tpname);
		     }
		   }
		   Delete(prefix);
		   inclass = 1;
		   currentOuterClass = (yyval.node);
		   if (cparse_cplusplusout) {
		     /* save the structure declaration to declare it in global scope for C++ to see */
		     code = get_raw_text_balanced('{', '}');
		     Setattr((yyval.node), "code", code);
		     Delete(code);
		   }
               }
    break;

  case 161:

/* Line 1464 of yacc.c  */
#line 3524 "parser.y"
    {
		   Node *p;
		   SwigType *ty;
		   Symtab *cscope;
		   Node *am = 0;
		   String *scpname = 0;
		   (void) (yyvsp[(6) - (9)].node);
		   (yyval.node) = currentOuterClass;
		   currentOuterClass = Getattr((yyval.node), "nested:outer");
		   nscope_inner = Getattr((yyval.node), "nested:innerscope");
		   nscope = Getattr((yyval.node), "nested:nscope");
		   Delattr((yyval.node), "nested:innerscope");
		   Delattr((yyval.node), "nested:nscope");
		   if (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0) { /* actual parent class for this class */
		     Node* forward_declaration = Swig_symbol_clookup_no_inherit(Getattr((yyval.node),"name"), Getattr(nscope_inner, "symtab"));
		     if (forward_declaration) {
		       Setattr((yyval.node), "access", Getattr(forward_declaration, "access"));
		     }
		     Setattr((yyval.node), "nested:outer", nscope_inner);
		     SetFlag((yyval.node), "nested");
                   }
		   if (!currentOuterClass)
		     inclass = 0;
		   cscope = Getattr((yyval.node), "prev_symtab");
		   Delattr((yyval.node), "prev_symtab");
		   
		   /* Check for pure-abstract class */
		   Setattr((yyval.node),"abstracts", pure_abstracts((yyvsp[(7) - (9)].node)));
		   
		   /* This bit of code merges in a previously defined %extend directive (if any) */
		   {
		     String *clsname = Swig_symbol_qualifiedscopename(0);
		     am = Getattr(Swig_extend_hash(), clsname);
		     if (am) {
		       Swig_extend_merge((yyval.node), am);
		       Delattr(Swig_extend_hash(), clsname);
		     }
		     Delete(clsname);
		   }
		   if (!classes) classes = NewHash();
		   scpname = Swig_symbol_qualifiedscopename(0);
		   Setattr(classes, scpname, (yyval.node));

		   appendChild((yyval.node), (yyvsp[(7) - (9)].node));
		   
		   if (am) 
		     Swig_extend_append_previous((yyval.node), am);

		   p = (yyvsp[(9) - (9)].node);
		   if (p && !nscope_inner) {
		     if (!cparse_cplusplus && currentOuterClass)
		       appendChild(currentOuterClass, p);
		     else
		      appendSibling((yyval.node), p);
		   }
		   
		   if (nscope_inner) {
		     ty = NewString(scpname); /* if the class is declared out of scope, let the declarator use fully qualified type*/
		   } else if (cparse_cplusplus && !cparse_externc) {
		     ty = NewString((yyvsp[(3) - (9)].str));
		   } else {
		     ty = NewStringf("%s %s", (yyvsp[(2) - (9)].id), (yyvsp[(3) - (9)].str));
		   }
		   while (p) {
		     Setattr(p, "storage", (yyvsp[(1) - (9)].id));
		     Setattr(p, "type" ,ty);
		     if (!cparse_cplusplus && currentOuterClass && (!Getattr(currentOuterClass, "name"))) {
		       SetFlag(p, "hasconsttype");
		       SetFlag(p, "feature:immutable");
		     }
		     p = nextSibling(p);
		   }
		   if ((yyvsp[(9) - (9)].node) && Cmp((yyvsp[(1) - (9)].id),"typedef") == 0)
		     add_typedef_name((yyval.node), (yyvsp[(9) - (9)].node), (yyvsp[(3) - (9)].str), cscope, scpname);
		   Delete(scpname);

		   if (cplus_mode != CPLUS_PUBLIC) {
		   /* we 'open' the class at the end, to allow %template
		      to add new members */
		     Node *pa = new_node("access");
		     Setattr(pa, "kind", "public");
		     cplus_mode = CPLUS_PUBLIC;
		     appendChild((yyval.node), pa);
		     Delete(pa);
		   }
		   if (currentOuterClass)
		     restore_access_mode((yyval.node));
		   Setattr((yyval.node), "symtab", Swig_symbol_popscope());
		   Classprefix = Getattr((yyval.node), "Classprefix");
		   Delattr((yyval.node), "Classprefix");
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   if (cplus_mode == CPLUS_PRIVATE) {
		     (yyval.node) = 0; /* skip private nested classes */
		   } else if (cparse_cplusplus && currentOuterClass && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested")) {
		     (yyval.node) = nested_forward_declaration((yyvsp[(1) - (9)].id), (yyvsp[(2) - (9)].id), (yyvsp[(3) - (9)].str), Copy((yyvsp[(3) - (9)].str)), (yyvsp[(9) - (9)].node));
		   } else if (nscope_inner) {
		     /* this is tricky */
		     /* we add the declaration in the original namespace */
		     if (Strcmp(nodeType(nscope_inner), "class") == 0 && cparse_cplusplus && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested"))
		       (yyval.node) = nested_forward_declaration((yyvsp[(1) - (9)].id), (yyvsp[(2) - (9)].id), (yyvsp[(3) - (9)].str), Copy((yyvsp[(3) - (9)].str)), (yyvsp[(9) - (9)].node));
		     appendChild(nscope_inner, (yyval.node));
		     Swig_symbol_setscope(Getattr(nscope_inner, "symtab"));
		     Delete(Namespaceprefix);
		     Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		     yyrename = Copy(Getattr((yyval.node), "class_rename"));
		     add_symbols((yyval.node));
		     Delattr((yyval.node), "class_rename");
		     /* but the variable definition in the current scope */
		     Swig_symbol_setscope(cscope);
		     Delete(Namespaceprefix);
		     Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		     add_symbols((yyvsp[(9) - (9)].node));
		     if (nscope) {
		       (yyval.node) = nscope; /* here we return recreated namespace tower instead of the class itself */
		       if ((yyvsp[(9) - (9)].node)) {
			 appendSibling((yyval.node), (yyvsp[(9) - (9)].node));
		       }
		     } else if (!SwigType_istemplate(ty) && template_parameters == 0) { /* for tempalte we need the class itself */
		       (yyval.node) = (yyvsp[(9) - (9)].node);
		     }
		   } else {
		     Delete(yyrename);
		     yyrename = 0;
		     if (!cparse_cplusplus && currentOuterClass) { /* nested C structs go into global scope*/
		       Node *outer = currentOuterClass;
		       while (Getattr(outer, "nested:outer"))
			 outer = Getattr(outer, "nested:outer");
		       appendSibling(outer, (yyval.node));
		       add_symbols((yyvsp[(9) - (9)].node));
		       set_scope_to_global();
		       Delete(Namespaceprefix);
		       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		       yyrename = Copy(Getattr((yyval.node), "class_rename"));
		       add_symbols((yyval.node));
		       if (!cparse_cplusplusout)
			 Delattr((yyval.node), "nested:outer");
		       Delattr((yyval.node), "class_rename");
		       (yyval.node) = 0;
		     } else {
		       yyrename = Copy(Getattr((yyval.node), "class_rename"));
		       add_symbols((yyval.node));
		       add_symbols((yyvsp[(9) - (9)].node));
		       Delattr((yyval.node), "class_rename");
		     }
		   }
		   Delete(ty);
		   Swig_symbol_setscope(cscope);
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   Classprefix = currentOuterClass ? Getattr(currentOuterClass, "Classprefix") : 0;
	       }
    break;

  case 162:

/* Line 1464 of yacc.c  */
#line 3679 "parser.y"
    {
	       String *unnamed;
	       String *code;
	       unnamed = make_unnamed();
	       (yyval.node) = new_node("class");
	       Setline((yyval.node),cparse_start_line);
	       Setattr((yyval.node),"kind",(yyvsp[(2) - (4)].id));
	       if ((yyvsp[(3) - (4)].bases)) {
		 Setattr((yyval.node),"baselist", Getattr((yyvsp[(3) - (4)].bases),"public"));
		 Setattr((yyval.node),"protectedbaselist", Getattr((yyvsp[(3) - (4)].bases),"protected"));
		 Setattr((yyval.node),"privatebaselist", Getattr((yyvsp[(3) - (4)].bases),"private"));
	       }
	       Setattr((yyval.node),"storage",(yyvsp[(1) - (4)].id));
	       Setattr((yyval.node),"unnamed",unnamed);
	       Setattr((yyval.node),"allows_typedef","1");
	       if (currentOuterClass) {
		 SetFlag((yyval.node), "nested");
		 Setattr((yyval.node), "nested:outer", currentOuterClass);
		 set_access_mode((yyval.node));
	       }
	       Swig_features_get(Swig_cparse_features(), Namespaceprefix, 0, 0, (yyval.node));
	       /* save yyrename to the class attribute, to be used later in add_symbols()*/
	       Setattr((yyval.node), "class_rename", make_name((yyval.node),0,0));
	       if (strcmp((yyvsp[(2) - (4)].id),"class") == 0) {
		 cplus_mode = CPLUS_PRIVATE;
	       } else {
		 cplus_mode = CPLUS_PUBLIC;
	       }
	       Swig_symbol_newscope();
	       cparse_start_line = cparse_line;
	       currentOuterClass = (yyval.node);
	       inclass = 1;
	       Classprefix = 0;
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       /* save the structure declaration to make a typedef for it later*/
	       code = get_raw_text_balanced('{', '}');
	       Setattr((yyval.node), "code", code);
	       Delete(code);
	     }
    break;

  case 163:

/* Line 1464 of yacc.c  */
#line 3718 "parser.y"
    {
	       String *unnamed;
               List *bases = 0;
	       String *name = 0;
	       Node *n;
	       Classprefix = 0;
	       (void)(yyvsp[(5) - (8)].node);
	       (yyval.node) = currentOuterClass;
	       currentOuterClass = Getattr((yyval.node), "nested:outer");
	       if (!currentOuterClass)
		 inclass = 0;
	       else
		 restore_access_mode((yyval.node));
	       unnamed = Getattr((yyval.node),"unnamed");
               /* Check for pure-abstract class */
	       Setattr((yyval.node),"abstracts", pure_abstracts((yyvsp[(6) - (8)].node)));
	       n = (yyvsp[(8) - (8)].node);
	       if (cparse_cplusplus && currentOuterClass && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested")) {
		 String *name = n ? Copy(Getattr(n, "name")) : 0;
		 (yyval.node) = nested_forward_declaration((yyvsp[(1) - (8)].id), (yyvsp[(2) - (8)].id), 0, name, n);
		 Swig_symbol_popscope();
	         Delete(Namespaceprefix);
		 Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       } else if (n) {
	         appendSibling((yyval.node),n);
		 /* If a proper typedef name was given, we'll use it to set the scope name */
		 name = try_to_find_a_name_for_unnamed_structure((yyvsp[(1) - (8)].id), n);
		 if (name) {
		   String *scpname = 0;
		   SwigType *ty;
		   Setattr((yyval.node),"tdname",name);
		   Setattr((yyval.node),"name",name);
		   Swig_symbol_setscopename(name);
		   if ((yyvsp[(3) - (8)].bases))
		     bases = Swig_make_inherit_list(name,Getattr((yyvsp[(3) - (8)].bases),"public"),Namespaceprefix);
		   Swig_inherit_base_symbols(bases);

		     /* If a proper name was given, we use that as the typedef, not unnamed */
		   Clear(unnamed);
		   Append(unnamed, name);
		   if (cparse_cplusplus && !cparse_externc) {
		     ty = NewString(name);
		   } else {
		     ty = NewStringf("%s %s", (yyvsp[(2) - (8)].id),name);
		   }
		   while (n) {
		     Setattr(n,"storage",(yyvsp[(1) - (8)].id));
		     Setattr(n, "type", ty);
		     if (!cparse_cplusplus && currentOuterClass && (!Getattr(currentOuterClass, "name"))) {
		       SetFlag(n,"hasconsttype");
		       SetFlag(n,"feature:immutable");
		     }
		     n = nextSibling(n);
		   }
		   n = (yyvsp[(8) - (8)].node);

		   /* Check for previous extensions */
		   {
		     String *clsname = Swig_symbol_qualifiedscopename(0);
		     Node *am = Getattr(Swig_extend_hash(),clsname);
		     if (am) {
		       /* Merge the extension into the symbol table */
		       Swig_extend_merge((yyval.node),am);
		       Swig_extend_append_previous((yyval.node),am);
		       Delattr(Swig_extend_hash(),clsname);
		     }
		     Delete(clsname);
		   }
		   if (!classes) classes = NewHash();
		   scpname = Swig_symbol_qualifiedscopename(0);
		   Setattr(classes,scpname,(yyval.node));
		   Delete(scpname);
		 } else { /* no suitable name was found for a struct */
		   Setattr((yyval.node), "nested:unnamed", Getattr(n, "name")); /* save the name of the first declarator for later use in name generation*/
		   while (n) { /* attach unnamed struct to the declarators, so that they would receive proper type later*/
		     Setattr(n, "nested:unnamedtype", (yyval.node));
		     Setattr(n, "storage", (yyvsp[(1) - (8)].id));
		     n = nextSibling(n);
		   }
		   n = (yyvsp[(8) - (8)].node);
		   Swig_symbol_setscopename("<unnamed>");
		 }
		 appendChild((yyval.node),(yyvsp[(6) - (8)].node));
		 /* Pop the scope */
		 Setattr((yyval.node),"symtab",Swig_symbol_popscope());
		 if (name) {
		   Delete(yyrename);
		   yyrename = Copy(Getattr((yyval.node), "class_rename"));
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   add_symbols((yyval.node));
		   add_symbols(n);
		   Delattr((yyval.node), "class_rename");
		 }else if (cparse_cplusplus)
		   (yyval.node) = 0; /* ignore unnamed structs for C++ */
	         Delete(unnamed);
	       } else { /* unnamed struct w/o declarator*/
		 Swig_symbol_popscope();
	         Delete(Namespaceprefix);
		 Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 add_symbols((yyvsp[(6) - (8)].node));
		 Delete((yyval.node));
		 (yyval.node) = (yyvsp[(6) - (8)].node); /* pass member list to outer class/namespace (instead of self)*/
	       }
	       Classprefix = currentOuterClass ? Getattr(currentOuterClass, "Classprefix") : 0;
              }
    break;

  case 164:

/* Line 1464 of yacc.c  */
#line 3826 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 165:

/* Line 1464 of yacc.c  */
#line 3827 "parser.y"
    {
                        (yyval.node) = new_node("cdecl");
                        Setattr((yyval.node),"name",(yyvsp[(1) - (3)].decl).id);
                        Setattr((yyval.node),"decl",(yyvsp[(1) - (3)].decl).type);
                        Setattr((yyval.node),"parms",(yyvsp[(1) - (3)].decl).parms);
			set_nextSibling((yyval.node),(yyvsp[(3) - (3)].node));
                    }
    break;

  case 166:

/* Line 1464 of yacc.c  */
#line 3839 "parser.y"
    {
              if ((yyvsp[(1) - (4)].id) && (Strcmp((yyvsp[(1) - (4)].id),"friend") == 0)) {
		/* Ignore */
                (yyval.node) = 0; 
	      } else {
		(yyval.node) = new_node("classforward");
		Setattr((yyval.node),"kind",(yyvsp[(2) - (4)].id));
		Setattr((yyval.node),"name",(yyvsp[(3) - (4)].str));
		Setattr((yyval.node),"sym:weak", "1");
		add_symbols((yyval.node));
	      }
             }
    break;

  case 167:

/* Line 1464 of yacc.c  */
#line 3857 "parser.y"
    { 
		   if (currentOuterClass)
		     Setattr(currentOuterClass, "template_parameters", template_parameters);
		    template_parameters = (yyvsp[(3) - (4)].tparms); 
		  }
    break;

  case 168:

/* Line 1464 of yacc.c  */
#line 3861 "parser.y"
    {
			String *tname = 0;
			int     error = 0;

			/* check if we get a namespace node with a class declaration, and retrieve the class */
			Symtab *cscope = Swig_symbol_current();
			Symtab *sti = 0;
			Node *ntop = (yyvsp[(6) - (6)].node);
			Node *ni = ntop;
			SwigType *ntype = ni ? nodeType(ni) : 0;
			while (ni && Strcmp(ntype,"namespace") == 0) {
			  sti = Getattr(ni,"symtab");
			  ni = firstChild(ni);
			  ntype = nodeType(ni);
			}
			if (sti) {
			  Swig_symbol_setscope(sti);
			  Delete(Namespaceprefix);
			  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
			  (yyvsp[(6) - (6)].node) = ni;
			}

			(yyval.node) = (yyvsp[(6) - (6)].node);
			if ((yyval.node)) tname = Getattr((yyval.node),"name");
			
			/* Check if the class is a template specialization */
			if (((yyval.node)) && (Strchr(tname,'<')) && (!is_operator(tname))) {
			  /* If a specialization.  Check if defined. */
			  Node *tempn = 0;
			  {
			    String *tbase = SwigType_templateprefix(tname);
			    tempn = Swig_symbol_clookup_local(tbase,0);
			    if (!tempn || (Strcmp(nodeType(tempn),"template") != 0)) {
			      SWIG_WARN_NODE_BEGIN(tempn);
			      Swig_warning(WARN_PARSE_TEMPLATE_SP_UNDEF, Getfile((yyval.node)),Getline((yyval.node)),"Specialization of non-template '%s'.\n", tbase);
			      SWIG_WARN_NODE_END(tempn);
			      tempn = 0;
			      error = 1;
			    }
			    Delete(tbase);
			  }
			  Setattr((yyval.node),"specialization","1");
			  Setattr((yyval.node),"templatetype",nodeType((yyval.node)));
			  set_nodeType((yyval.node),"template");
			  /* Template partial specialization */
			  if (tempn && ((yyvsp[(3) - (6)].tparms)) && ((yyvsp[(6) - (6)].node))) {
			    List   *tlist;
			    String *targs = SwigType_templateargs(tname);
			    tlist = SwigType_parmlist(targs);
			    /*			  Printf(stdout,"targs = '%s' %s\n", targs, tlist); */
			    if (!Getattr((yyval.node),"sym:weak")) {
			      Setattr((yyval.node),"sym:typename","1");
			    }
			    
			    if (Len(tlist) != ParmList_len(Getattr(tempn,"templateparms"))) {
			      Swig_error(Getfile((yyval.node)),Getline((yyval.node)),"Inconsistent argument count in template partial specialization. %d %d\n", Len(tlist), ParmList_len(Getattr(tempn,"templateparms")));
			      
			    } else {

			    /* This code builds the argument list for the partial template
			       specialization.  This is a little hairy, but the idea is as
			       follows:

			       $3 contains a list of arguments supplied for the template.
			       For example template<class T>.

			       tlist is a list of the specialization arguments--which may be
			       different.  For example class<int,T>.

			       tp is a copy of the arguments in the original template definition.
       
			       The patching algorithm walks through the list of supplied
			       arguments ($3), finds the position in the specialization arguments
			       (tlist), and then patches the name in the argument list of the
			       original template.
			    */

			    {
			      String *pn;
			      Parm *p, *p1;
			      int i, nargs;
			      Parm *tp = CopyParmList(Getattr(tempn,"templateparms"));
			      nargs = Len(tlist);
			      p = (yyvsp[(3) - (6)].tparms);
			      while (p) {
				for (i = 0; i < nargs; i++){
				  pn = Getattr(p,"name");
				  if (Strcmp(pn,SwigType_base(Getitem(tlist,i))) == 0) {
				    int j;
				    Parm *p1 = tp;
				    for (j = 0; j < i; j++) {
				      p1 = nextSibling(p1);
				    }
				    Setattr(p1,"name",pn);
				    Setattr(p1,"partialarg","1");
				  }
				}
				p = nextSibling(p);
			      }
			      p1 = tp;
			      i = 0;
			      while (p1) {
				if (!Getattr(p1,"partialarg")) {
				  Delattr(p1,"name");
				  Setattr(p1,"type", Getitem(tlist,i));
				} 
				i++;
				p1 = nextSibling(p1);
			      }
			      Setattr((yyval.node),"templateparms",tp);
			      Delete(tp);
			    }
  #if 0
			    /* Patch the parameter list */
			    if (tempn) {
			      Parm *p,*p1;
			      ParmList *tp = CopyParmList(Getattr(tempn,"templateparms"));
			      p = (yyvsp[(3) - (6)].tparms);
			      p1 = tp;
			      while (p && p1) {
				String *pn = Getattr(p,"name");
				Printf(stdout,"pn = '%s'\n", pn);
				if (pn) Setattr(p1,"name",pn);
				else Delattr(p1,"name");
				pn = Getattr(p,"type");
				if (pn) Setattr(p1,"type",pn);
				p = nextSibling(p);
				p1 = nextSibling(p1);
			      }
			      Setattr((yyval.node),"templateparms",tp);
			      Delete(tp);
			    } else {
			      Setattr((yyval.node),"templateparms",(yyvsp[(3) - (6)].tparms));
			    }
  #endif
			    Delattr((yyval.node),"specialization");
			    Setattr((yyval.node),"partialspecialization","1");
			    /* Create a specialized name for matching */
			    {
			      Parm *p = (yyvsp[(3) - (6)].tparms);
			      String *fname = NewString(Getattr((yyval.node),"name"));
			      String *ffname = 0;
			      ParmList *partialparms = 0;

			      char   tmp[32];
			      int    i, ilen;
			      while (p) {
				String *n = Getattr(p,"name");
				if (!n) {
				  p = nextSibling(p);
				  continue;
				}
				ilen = Len(tlist);
				for (i = 0; i < ilen; i++) {
				  if (Strstr(Getitem(tlist,i),n)) {
				    sprintf(tmp,"$%d",i+1);
				    Replaceid(fname,n,tmp);
				  }
				}
				p = nextSibling(p);
			      }
			      /* Patch argument names with typedef */
			      {
				Iterator tt;
				Parm *parm_current = 0;
				List *tparms = SwigType_parmlist(fname);
				ffname = SwigType_templateprefix(fname);
				Append(ffname,"<(");
				for (tt = First(tparms); tt.item; ) {
				  SwigType *rtt = Swig_symbol_typedef_reduce(tt.item,0);
				  SwigType *ttr = Swig_symbol_type_qualify(rtt,0);

				  Parm *newp = NewParmWithoutFileLineInfo(ttr, 0);
				  if (partialparms)
				    set_nextSibling(parm_current, newp);
				  else
				    partialparms = newp;
				  parm_current = newp;

				  Append(ffname,ttr);
				  tt = Next(tt);
				  if (tt.item) Putc(',',ffname);
				  Delete(rtt);
				  Delete(ttr);
				}
				Delete(tparms);
				Append(ffname,")>");
			      }
			      {
				Node *new_partial = NewHash();
				String *partials = Getattr(tempn,"partials");
				if (!partials) {
				  partials = NewList();
				  Setattr(tempn,"partials",partials);
				  Delete(partials);
				}
				/*			      Printf(stdout,"partial: fname = '%s', '%s'\n", fname, Swig_symbol_typedef_reduce(fname,0)); */
				Setattr(new_partial, "partialparms", partialparms);
				Setattr(new_partial, "templcsymname", ffname);
				Append(partials, new_partial);
			      }
			      Setattr((yyval.node),"partialargs",ffname);
			      Swig_symbol_cadd(ffname,(yyval.node));
			    }
			    }
			    Delete(tlist);
			    Delete(targs);
			  } else {
			    /* An explicit template specialization */
			    /* add default args from primary (unspecialized) template */
			    String *ty = Swig_symbol_template_deftype(tname,0);
			    String *fname = Swig_symbol_type_qualify(ty,0);
			    Swig_symbol_cadd(fname,(yyval.node));
			    Delete(ty);
			    Delete(fname);
			  }
			}  else if ((yyval.node)) {
			  Setattr((yyval.node),"templatetype",nodeType((yyvsp[(6) - (6)].node)));
			  set_nodeType((yyval.node),"template");
			  Setattr((yyval.node),"templateparms", (yyvsp[(3) - (6)].tparms));
			  if (!Getattr((yyval.node),"sym:weak")) {
			    Setattr((yyval.node),"sym:typename","1");
			  }
			  add_symbols((yyval.node));
			  default_arguments((yyval.node));
			  /* We also place a fully parameterized version in the symbol table */
			  {
			    Parm *p;
			    String *fname = NewStringf("%s<(", Getattr((yyval.node),"name"));
			    p = (yyvsp[(3) - (6)].tparms);
			    while (p) {
			      String *n = Getattr(p,"name");
			      if (!n) n = Getattr(p,"type");
			      Append(fname,n);
			      p = nextSibling(p);
			      if (p) Putc(',',fname);
			    }
			    Append(fname,")>");
			    Swig_symbol_cadd(fname,(yyval.node));
			  }
			}
			(yyval.node) = ntop;
			Swig_symbol_setscope(cscope);
			Delete(Namespaceprefix);
			Namespaceprefix = Swig_symbol_qualifiedscopename(0);
			if (error || (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0)) {
			  (yyval.node) = 0;
			}
			if (currentOuterClass)
			  template_parameters = Getattr(currentOuterClass, "template_parameters");
			else
			  template_parameters = 0;
                }
    break;

  case 169:

/* Line 1464 of yacc.c  */
#line 4116 "parser.y"
    {
		  Swig_warning(WARN_PARSE_EXPLICIT_TEMPLATE, cparse_file, cparse_line, "Explicit template instantiation ignored.\n");
                  (yyval.node) = 0; 
		}
    break;

  case 170:

/* Line 1464 of yacc.c  */
#line 4122 "parser.y"
    {
		  Swig_warning(WARN_PARSE_EXPLICIT_TEMPLATE, cparse_file, cparse_line, "Explicit template instantiation ignored.\n");
                  (yyval.node) = 0; 
                }
    break;

  case 171:

/* Line 1464 of yacc.c  */
#line 4128 "parser.y"
    {
		  (yyval.node) = (yyvsp[(1) - (1)].node);
                }
    break;

  case 172:

/* Line 1464 of yacc.c  */
#line 4131 "parser.y"
    {
                   (yyval.node) = (yyvsp[(1) - (1)].node);
                }
    break;

  case 173:

/* Line 1464 of yacc.c  */
#line 4134 "parser.y"
    {
                   (yyval.node) = (yyvsp[(1) - (1)].node);
                }
    break;

  case 174:

/* Line 1464 of yacc.c  */
#line 4137 "parser.y"
    {
                   (yyval.node) = (yyvsp[(1) - (1)].node);
                }
    break;

  case 175:

/* Line 1464 of yacc.c  */
#line 4140 "parser.y"
    {
		  (yyval.node) = 0;
                }
    break;

  case 176:

/* Line 1464 of yacc.c  */
#line 4143 "parser.y"
    {
                  (yyval.node) = (yyvsp[(1) - (1)].node);
                }
    break;

  case 177:

/* Line 1464 of yacc.c  */
#line 4146 "parser.y"
    {
                  (yyval.node) = (yyvsp[(1) - (1)].node);
                }
    break;

  case 178:

/* Line 1464 of yacc.c  */
#line 4151 "parser.y"
    {
		   /* Rip out the parameter names */
		  Parm *p = (yyvsp[(1) - (1)].pl);
		  (yyval.tparms) = (yyvsp[(1) - (1)].pl);

		  while (p) {
		    String *name = Getattr(p,"name");
		    if (!name) {
		      /* Hmmm. Maybe it's a 'class T' parameter */
		      char *type = Char(Getattr(p,"type"));
		      /* Template template parameter */
		      if (strncmp(type,"template<class> ",16) == 0) {
			type += 16;
		      }
		      if ((strncmp(type,"class ",6) == 0) || (strncmp(type,"typename ", 9) == 0)) {
			char *t = strchr(type,' ');
			Setattr(p,"name", t+1);
		      } else 
                      /* Variadic template args */
		      if ((strncmp(type,"class... ",9) == 0) || (strncmp(type,"typename... ", 12) == 0)) {
			char *t = strchr(type,' ');
			Setattr(p,"name", t+1);
			Setattr(p,"variadic", "1");
		      } else {
			/*
			 Swig_error(cparse_file, cparse_line, "Missing template parameter name\n");
			 $$.rparms = 0;
			 $$.parms = 0;
			 break; */
		      }
		    }
		    p = nextSibling(p);
		  }
                 }
    break;

  case 179:

/* Line 1464 of yacc.c  */
#line 4187 "parser.y"
    {
                      set_nextSibling((yyvsp[(1) - (2)].p),(yyvsp[(2) - (2)].pl));
                      (yyval.pl) = (yyvsp[(1) - (2)].p);
                   }
    break;

  case 180:

/* Line 1464 of yacc.c  */
#line 4191 "parser.y"
    { (yyval.pl) = 0; }
    break;

  case 181:

/* Line 1464 of yacc.c  */
#line 4194 "parser.y"
    {
		    (yyval.p) = NewParmWithoutFileLineInfo(NewString((yyvsp[(1) - (1)].id)), 0);
                  }
    break;

  case 182:

/* Line 1464 of yacc.c  */
#line 4197 "parser.y"
    {
                    (yyval.p) = (yyvsp[(1) - (1)].p);
                  }
    break;

  case 183:

/* Line 1464 of yacc.c  */
#line 4202 "parser.y"
    {
                         set_nextSibling((yyvsp[(2) - (3)].p),(yyvsp[(3) - (3)].pl));
                         (yyval.pl) = (yyvsp[(2) - (3)].p);
                       }
    break;

  case 184:

/* Line 1464 of yacc.c  */
#line 4206 "parser.y"
    { (yyval.pl) = 0; }
    break;

  case 185:

/* Line 1464 of yacc.c  */
#line 4211 "parser.y"
    {
                  String *uname = Swig_symbol_type_qualify((yyvsp[(2) - (3)].str),0);
		  String *name = Swig_scopename_last((yyvsp[(2) - (3)].str));
                  (yyval.node) = new_node("using");
		  Setattr((yyval.node),"uname",uname);
		  Setattr((yyval.node),"name", name);
		  Delete(uname);
		  Delete(name);
		  add_symbols((yyval.node));
             }
    break;

  case 186:

/* Line 1464 of yacc.c  */
#line 4221 "parser.y"
    {
	       Node *n = Swig_symbol_clookup((yyvsp[(3) - (4)].str),0);
	       if (!n) {
		 Swig_error(cparse_file, cparse_line, "Nothing known about namespace '%s'\n", (yyvsp[(3) - (4)].str));
		 (yyval.node) = 0;
	       } else {

		 while (Strcmp(nodeType(n),"using") == 0) {
		   n = Getattr(n,"node");
		 }
		 if (n) {
		   if (Strcmp(nodeType(n),"namespace") == 0) {
		     Symtab *current = Swig_symbol_current();
		     Symtab *symtab = Getattr(n,"symtab");
		     (yyval.node) = new_node("using");
		     Setattr((yyval.node),"node",n);
		     Setattr((yyval.node),"namespace", (yyvsp[(3) - (4)].str));
		     if (current != symtab) {
		       Swig_symbol_inherit(symtab);
		     }
		   } else {
		     Swig_error(cparse_file, cparse_line, "'%s' is not a namespace.\n", (yyvsp[(3) - (4)].str));
		     (yyval.node) = 0;
		   }
		 } else {
		   (yyval.node) = 0;
		 }
	       }
             }
    break;

  case 187:

/* Line 1464 of yacc.c  */
#line 4252 "parser.y"
    { 
                Hash *h;
                (yyvsp[(1) - (3)].node) = Swig_symbol_current();
		h = Swig_symbol_clookup((yyvsp[(2) - (3)].str),0);
		if (h && ((yyvsp[(1) - (3)].node) == Getattr(h,"sym:symtab")) && (Strcmp(nodeType(h),"namespace") == 0)) {
		  if (Getattr(h,"alias")) {
		    h = Getattr(h,"namespace");
		    Swig_warning(WARN_PARSE_NAMESPACE_ALIAS, cparse_file, cparse_line, "Namespace alias '%s' not allowed here. Assuming '%s'\n",
				 (yyvsp[(2) - (3)].str), Getattr(h,"name"));
		    (yyvsp[(2) - (3)].str) = Getattr(h,"name");
		  }
		  Swig_symbol_setscope(Getattr(h,"symtab"));
		} else {
		  Swig_symbol_newscope();
		  Swig_symbol_setscopename((yyvsp[(2) - (3)].str));
		}
		Delete(Namespaceprefix);
		Namespaceprefix = Swig_symbol_qualifiedscopename(0);
             }
    break;

  case 188:

/* Line 1464 of yacc.c  */
#line 4270 "parser.y"
    {
                Node *n = (yyvsp[(5) - (6)].node);
		set_nodeType(n,"namespace");
		Setattr(n,"name",(yyvsp[(2) - (6)].str));
                Setattr(n,"symtab", Swig_symbol_popscope());
		Swig_symbol_setscope((yyvsp[(1) - (6)].node));
		(yyval.node) = n;
		Delete(Namespaceprefix);
		Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		add_symbols((yyval.node));
             }
    break;

  case 189:

/* Line 1464 of yacc.c  */
#line 4281 "parser.y"
    {
	       Hash *h;
	       (yyvsp[(1) - (2)].node) = Swig_symbol_current();
	       h = Swig_symbol_clookup("    ",0);
	       if (h && (Strcmp(nodeType(h),"namespace") == 0)) {
		 Swig_symbol_setscope(Getattr(h,"symtab"));
	       } else {
		 Swig_symbol_newscope();
		 /* we don't use "__unnamed__", but a long 'empty' name */
		 Swig_symbol_setscopename("    ");
	       }
	       Namespaceprefix = 0;
             }
    break;

  case 190:

/* Line 1464 of yacc.c  */
#line 4293 "parser.y"
    {
	       (yyval.node) = (yyvsp[(4) - (5)].node);
	       set_nodeType((yyval.node),"namespace");
	       Setattr((yyval.node),"unnamed","1");
	       Setattr((yyval.node),"symtab", Swig_symbol_popscope());
	       Swig_symbol_setscope((yyvsp[(1) - (5)].node));
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       add_symbols((yyval.node));
             }
    break;

  case 191:

/* Line 1464 of yacc.c  */
#line 4303 "parser.y"
    {
	       /* Namespace alias */
	       Node *n;
	       (yyval.node) = new_node("namespace");
	       Setattr((yyval.node),"name",(yyvsp[(2) - (5)].id));
	       Setattr((yyval.node),"alias",(yyvsp[(4) - (5)].str));
	       n = Swig_symbol_clookup((yyvsp[(4) - (5)].str),0);
	       if (!n) {
		 Swig_error(cparse_file, cparse_line, "Unknown namespace '%s'\n", (yyvsp[(4) - (5)].str));
		 (yyval.node) = 0;
	       } else {
		 if (Strcmp(nodeType(n),"namespace") != 0) {
		   Swig_error(cparse_file, cparse_line, "'%s' is not a namespace\n",(yyvsp[(4) - (5)].str));
		   (yyval.node) = 0;
		 } else {
		   while (Getattr(n,"alias")) {
		     n = Getattr(n,"namespace");
		   }
		   Setattr((yyval.node),"namespace",n);
		   add_symbols((yyval.node));
		   /* Set up a scope alias */
		   Swig_symbol_alias((yyvsp[(2) - (5)].id),Getattr(n,"symtab"));
		 }
	       }
             }
    break;

  case 192:

/* Line 1464 of yacc.c  */
#line 4330 "parser.y"
    {
                   (yyval.node) = (yyvsp[(1) - (2)].node);
                   /* Insert cpp_member (including any siblings) to the front of the cpp_members linked list */
		   if ((yyval.node)) {
		     Node *p = (yyval.node);
		     Node *pp =0;
		     while (p) {
		       pp = p;
		       p = nextSibling(p);
		     }
		     set_nextSibling(pp,(yyvsp[(2) - (2)].node));
		     if ((yyvsp[(2) - (2)].node))
		       set_previousSibling((yyvsp[(2) - (2)].node), pp);
		   } else {
		     (yyval.node) = (yyvsp[(2) - (2)].node);
		   }
             }
    break;

  case 193:

/* Line 1464 of yacc.c  */
#line 4347 "parser.y"
    { 
	       extendmode = 1;
	       if (cplus_mode != CPLUS_PUBLIC) {
		 Swig_error(cparse_file,cparse_line,"%%extend can only be used in a public section\n");
	       }
             }
    break;

  case 194:

/* Line 1464 of yacc.c  */
#line 4352 "parser.y"
    {
	       extendmode = 0;
	     }
    break;

  case 195:

/* Line 1464 of yacc.c  */
#line 4354 "parser.y"
    {
	       (yyval.node) = new_node("extend");
	       mark_nodes_as_extend((yyvsp[(4) - (7)].node));
	       appendChild((yyval.node),(yyvsp[(4) - (7)].node));
	       set_nextSibling((yyval.node),(yyvsp[(7) - (7)].node));
	     }
    break;

  case 196:

/* Line 1464 of yacc.c  */
#line 4360 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 197:

/* Line 1464 of yacc.c  */
#line 4361 "parser.y"
    { (yyval.node) = 0;}
    break;

  case 198:

/* Line 1464 of yacc.c  */
#line 4362 "parser.y"
    {
	       int start_line = cparse_line;
	       skip_decl();
	       Swig_error(cparse_file,start_line,"Syntax error in input(3).\n");
	       exit(1);
	       }
    break;

  case 199:

/* Line 1464 of yacc.c  */
#line 4367 "parser.y"
    { 
		 (yyval.node) = (yyvsp[(3) - (3)].node);
   	     }
    break;

  case 200:

/* Line 1464 of yacc.c  */
#line 4378 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 201:

/* Line 1464 of yacc.c  */
#line 4379 "parser.y"
    { 
                 (yyval.node) = (yyvsp[(1) - (1)].node); 
		 if (extendmode && current_class) {
		   String *symname;
		   symname= make_name((yyval.node),Getattr((yyval.node),"name"), Getattr((yyval.node),"decl"));
		   if (Strcmp(symname,Getattr((yyval.node),"name")) == 0) {
		     /* No renaming operation.  Set name to class name */
		     Delete(yyrename);
		     yyrename = NewString(Getattr(current_class,"sym:name"));
		   } else {
		     Delete(yyrename);
		     yyrename = symname;
		   }
		 }
		 add_symbols((yyval.node));
                 default_arguments((yyval.node));
             }
    break;

  case 202:

/* Line 1464 of yacc.c  */
#line 4396 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 203:

/* Line 1464 of yacc.c  */
#line 4397 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 204:

/* Line 1464 of yacc.c  */
#line 4398 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 205:

/* Line 1464 of yacc.c  */
#line 4399 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 206:

/* Line 1464 of yacc.c  */
#line 4400 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 207:

/* Line 1464 of yacc.c  */
#line 4401 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 208:

/* Line 1464 of yacc.c  */
#line 4402 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 209:

/* Line 1464 of yacc.c  */
#line 4403 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 210:

/* Line 1464 of yacc.c  */
#line 4404 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 211:

/* Line 1464 of yacc.c  */
#line 4405 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 212:

/* Line 1464 of yacc.c  */
#line 4406 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 213:

/* Line 1464 of yacc.c  */
#line 4407 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 214:

/* Line 1464 of yacc.c  */
#line 4408 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 215:

/* Line 1464 of yacc.c  */
#line 4409 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 216:

/* Line 1464 of yacc.c  */
#line 4410 "parser.y"
    {(yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 217:

/* Line 1464 of yacc.c  */
#line 4411 "parser.y"
    {(yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 218:

/* Line 1464 of yacc.c  */
#line 4412 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 219:

/* Line 1464 of yacc.c  */
#line 4421 "parser.y"
    {
              if (inclass || extendmode) {
		SwigType *decl = NewStringEmpty();
		(yyval.node) = new_node("constructor");
		Setattr((yyval.node),"storage",(yyvsp[(1) - (6)].id));
		Setattr((yyval.node),"name",(yyvsp[(2) - (6)].type));
		Setattr((yyval.node),"parms",(yyvsp[(4) - (6)].pl));
		SwigType_add_function(decl,(yyvsp[(4) - (6)].pl));
		Setattr((yyval.node),"decl",decl);
		Setattr((yyval.node),"throws",(yyvsp[(6) - (6)].decl).throws);
		Setattr((yyval.node),"throw",(yyvsp[(6) - (6)].decl).throwf);
		Setattr((yyval.node),"noexcept",(yyvsp[(6) - (6)].decl).nexcept);
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
		SetFlag((yyval.node),"feature:new");
		if ((yyvsp[(6) - (6)].decl).defarg)
		  Setattr((yyval.node),"value",(yyvsp[(6) - (6)].decl).defarg);
	      } else {
		(yyval.node) = 0;
              }
              }
    break;

  case 220:

/* Line 1464 of yacc.c  */
#line 4449 "parser.y"
    {
               String *name = NewStringf("%s",(yyvsp[(2) - (6)].str));
	       if (*(Char(name)) != '~') Insert(name,0,"~");
               (yyval.node) = new_node("destructor");
	       Setattr((yyval.node),"name",name);
	       Delete(name);
	       if (Len(scanner_ccode)) {
		 String *code = Copy(scanner_ccode);
		 Setattr((yyval.node),"code",code);
		 Delete(code);
	       }
	       {
		 String *decl = NewStringEmpty();
		 SwigType_add_function(decl,(yyvsp[(4) - (6)].pl));
		 Setattr((yyval.node),"decl",decl);
		 Delete(decl);
	       }
	       Setattr((yyval.node),"throws",(yyvsp[(6) - (6)].dtype).throws);
	       Setattr((yyval.node),"throw",(yyvsp[(6) - (6)].dtype).throwf);
	       Setattr((yyval.node),"noexcept",(yyvsp[(6) - (6)].dtype).nexcept);
	       if ((yyvsp[(6) - (6)].dtype).val)
	         Setattr((yyval.node),"value",(yyvsp[(6) - (6)].dtype).val);
	       add_symbols((yyval.node));
	      }
    break;

  case 221:

/* Line 1464 of yacc.c  */
#line 4476 "parser.y"
    {
		String *name;
		(yyval.node) = new_node("destructor");
		Setattr((yyval.node),"storage","virtual");
	        name = NewStringf("%s",(yyvsp[(3) - (7)].str));
		if (*(Char(name)) != '~') Insert(name,0,"~");
		Setattr((yyval.node),"name",name);
		Delete(name);
		Setattr((yyval.node),"throws",(yyvsp[(7) - (7)].dtype).throws);
		Setattr((yyval.node),"throw",(yyvsp[(7) - (7)].dtype).throwf);
		Setattr((yyval.node),"noexcept",(yyvsp[(7) - (7)].dtype).nexcept);
		if ((yyvsp[(7) - (7)].dtype).val)
		  Setattr((yyval.node),"value",(yyvsp[(7) - (7)].dtype).val);
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
		{
		  String *decl = NewStringEmpty();
		  SwigType_add_function(decl,(yyvsp[(5) - (7)].pl));
		  Setattr((yyval.node),"decl",decl);
		  Delete(decl);
		}

		add_symbols((yyval.node));
	      }
    break;

  case 222:

/* Line 1464 of yacc.c  */
#line 4507 "parser.y"
    {
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[(3) - (8)].type));
		 Setattr((yyval.node),"name",(yyvsp[(2) - (8)].str));
		 Setattr((yyval.node),"storage",(yyvsp[(1) - (8)].id));

		 SwigType_add_function((yyvsp[(4) - (8)].type),(yyvsp[(6) - (8)].pl));
		 if ((yyvsp[(8) - (8)].dtype).qualifier) {
		   SwigType_push((yyvsp[(4) - (8)].type),(yyvsp[(8) - (8)].dtype).qualifier);
		 }
		 Setattr((yyval.node),"decl",(yyvsp[(4) - (8)].type));
		 Setattr((yyval.node),"parms",(yyvsp[(6) - (8)].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
              }
    break;

  case 223:

/* Line 1464 of yacc.c  */
#line 4522 "parser.y"
    {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[(3) - (8)].type));
		 Setattr((yyval.node),"name",(yyvsp[(2) - (8)].str));
		 Setattr((yyval.node),"storage",(yyvsp[(1) - (8)].id));
		 decl = NewStringEmpty();
		 SwigType_add_reference(decl);
		 SwigType_add_function(decl,(yyvsp[(6) - (8)].pl));
		 if ((yyvsp[(8) - (8)].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[(8) - (8)].dtype).qualifier);
		 }
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[(6) - (8)].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
	       }
    break;

  case 224:

/* Line 1464 of yacc.c  */
#line 4539 "parser.y"
    {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[(3) - (8)].type));
		 Setattr((yyval.node),"name",(yyvsp[(2) - (8)].str));
		 Setattr((yyval.node),"storage",(yyvsp[(1) - (8)].id));
		 decl = NewStringEmpty();
		 SwigType_add_rvalue_reference(decl);
		 SwigType_add_function(decl,(yyvsp[(6) - (8)].pl));
		 if ((yyvsp[(8) - (8)].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[(8) - (8)].dtype).qualifier);
		 }
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[(6) - (8)].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
	       }
    break;

  case 225:

/* Line 1464 of yacc.c  */
#line 4557 "parser.y"
    {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[(3) - (9)].type));
		 Setattr((yyval.node),"name",(yyvsp[(2) - (9)].str));
		 Setattr((yyval.node),"storage",(yyvsp[(1) - (9)].id));
		 decl = NewStringEmpty();
		 SwigType_add_pointer(decl);
		 SwigType_add_reference(decl);
		 SwigType_add_function(decl,(yyvsp[(7) - (9)].pl));
		 if ((yyvsp[(9) - (9)].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[(9) - (9)].dtype).qualifier);
		 }
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[(7) - (9)].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
	       }
    break;

  case 226:

/* Line 1464 of yacc.c  */
#line 4576 "parser.y"
    {
		String *t = NewStringEmpty();
		(yyval.node) = new_node("cdecl");
		Setattr((yyval.node),"type",(yyvsp[(3) - (7)].type));
		Setattr((yyval.node),"name",(yyvsp[(2) - (7)].str));
		 Setattr((yyval.node),"storage",(yyvsp[(1) - (7)].id));
		SwigType_add_function(t,(yyvsp[(5) - (7)].pl));
		if ((yyvsp[(7) - (7)].dtype).qualifier) {
		  SwigType_push(t,(yyvsp[(7) - (7)].dtype).qualifier);
		}
		Setattr((yyval.node),"decl",t);
		Setattr((yyval.node),"parms",(yyvsp[(5) - (7)].pl));
		Setattr((yyval.node),"conversion_operator","1");
		add_symbols((yyval.node));
              }
    break;

  case 227:

/* Line 1464 of yacc.c  */
#line 4595 "parser.y"
    {
                 skip_balanced('{','}');
                 (yyval.node) = 0;
               }
    break;

  case 228:

/* Line 1464 of yacc.c  */
#line 4602 "parser.y"
    {
                skip_balanced('(',')');
                (yyval.node) = 0;
              }
    break;

  case 229:

/* Line 1464 of yacc.c  */
#line 4609 "parser.y"
    { 
                (yyval.node) = new_node("access");
		Setattr((yyval.node),"kind","public");
                cplus_mode = CPLUS_PUBLIC;
              }
    break;

  case 230:

/* Line 1464 of yacc.c  */
#line 4616 "parser.y"
    { 
                (yyval.node) = new_node("access");
                Setattr((yyval.node),"kind","private");
		cplus_mode = CPLUS_PRIVATE;
	      }
    break;

  case 231:

/* Line 1464 of yacc.c  */
#line 4624 "parser.y"
    { 
		(yyval.node) = new_node("access");
		Setattr((yyval.node),"kind","protected");
		cplus_mode = CPLUS_PROTECTED;
	      }
    break;

  case 232:

/* Line 1464 of yacc.c  */
#line 4632 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 233:

/* Line 1464 of yacc.c  */
#line 4635 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 234:

/* Line 1464 of yacc.c  */
#line 4639 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 235:

/* Line 1464 of yacc.c  */
#line 4642 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 236:

/* Line 1464 of yacc.c  */
#line 4643 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 237:

/* Line 1464 of yacc.c  */
#line 4644 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 238:

/* Line 1464 of yacc.c  */
#line 4645 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 239:

/* Line 1464 of yacc.c  */
#line 4646 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 240:

/* Line 1464 of yacc.c  */
#line 4647 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 241:

/* Line 1464 of yacc.c  */
#line 4648 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 242:

/* Line 1464 of yacc.c  */
#line 4649 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 243:

/* Line 1464 of yacc.c  */
#line 4652 "parser.y"
    {
	            Clear(scanner_ccode);
		    (yyval.dtype).val = 0;
		    (yyval.dtype).throws = (yyvsp[(1) - (2)].dtype).throws;
		    (yyval.dtype).throwf = (yyvsp[(1) - (2)].dtype).throwf;
		    (yyval.dtype).nexcept = (yyvsp[(1) - (2)].dtype).nexcept;
               }
    break;

  case 244:

/* Line 1464 of yacc.c  */
#line 4659 "parser.y"
    {
	            Clear(scanner_ccode);
		    (yyval.dtype).val = (yyvsp[(3) - (4)].dtype).val;
		    (yyval.dtype).throws = (yyvsp[(1) - (4)].dtype).throws;
		    (yyval.dtype).throwf = (yyvsp[(1) - (4)].dtype).throwf;
		    (yyval.dtype).nexcept = (yyvsp[(1) - (4)].dtype).nexcept;
               }
    break;

  case 245:

/* Line 1464 of yacc.c  */
#line 4666 "parser.y"
    { 
		    skip_balanced('{','}'); 
		    (yyval.dtype).val = 0;
		    (yyval.dtype).throws = (yyvsp[(1) - (2)].dtype).throws;
		    (yyval.dtype).throwf = (yyvsp[(1) - (2)].dtype).throwf;
		    (yyval.dtype).nexcept = (yyvsp[(1) - (2)].dtype).nexcept;
	       }
    break;

  case 246:

/* Line 1464 of yacc.c  */
#line 4675 "parser.y"
    { 
                     Clear(scanner_ccode);
                     (yyval.dtype).val = 0;
                     (yyval.dtype).qualifier = (yyvsp[(1) - (2)].dtype).qualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[(1) - (2)].dtype).throws;
                     (yyval.dtype).throwf = (yyvsp[(1) - (2)].dtype).throwf;
                     (yyval.dtype).nexcept = (yyvsp[(1) - (2)].dtype).nexcept;
                }
    break;

  case 247:

/* Line 1464 of yacc.c  */
#line 4684 "parser.y"
    { 
                     Clear(scanner_ccode);
                     (yyval.dtype).val = (yyvsp[(3) - (4)].dtype).val;
                     (yyval.dtype).qualifier = (yyvsp[(1) - (4)].dtype).qualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[(1) - (4)].dtype).throws; 
                     (yyval.dtype).throwf = (yyvsp[(1) - (4)].dtype).throwf; 
                     (yyval.dtype).nexcept = (yyvsp[(1) - (4)].dtype).nexcept; 
               }
    break;

  case 248:

/* Line 1464 of yacc.c  */
#line 4693 "parser.y"
    { 
                     skip_balanced('{','}');
                     (yyval.dtype).val = 0;
                     (yyval.dtype).qualifier = (yyvsp[(1) - (2)].dtype).qualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[(1) - (2)].dtype).throws; 
                     (yyval.dtype).throwf = (yyvsp[(1) - (2)].dtype).throwf; 
                     (yyval.dtype).nexcept = (yyvsp[(1) - (2)].dtype).nexcept; 
               }
    break;

  case 249:

/* Line 1464 of yacc.c  */
#line 4705 "parser.y"
    { }
    break;

  case 250:

/* Line 1464 of yacc.c  */
#line 4708 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type);
                  /* Printf(stdout,"primitive = '%s'\n", $$);*/
                }
    break;

  case 251:

/* Line 1464 of yacc.c  */
#line 4711 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type); }
    break;

  case 252:

/* Line 1464 of yacc.c  */
#line 4712 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type); }
    break;

  case 253:

/* Line 1464 of yacc.c  */
#line 4716 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type); }
    break;

  case 254:

/* Line 1464 of yacc.c  */
#line 4718 "parser.y"
    {
		  (yyval.type) = (yyvsp[(1) - (1)].str);
               }
    break;

  case 255:

/* Line 1464 of yacc.c  */
#line 4726 "parser.y"
    {
                   if (Strcmp((yyvsp[(2) - (2)].str),"C") == 0) {
		     (yyval.id) = "externc";
                   } else if (Strcmp((yyvsp[(2) - (2)].str),"C++") == 0) {
		     (yyval.id) = "extern";
		   } else {
		     Swig_warning(WARN_PARSE_UNDEFINED_EXTERN,cparse_file, cparse_line,"Unrecognized extern type \"%s\".\n", (yyvsp[(2) - (2)].str));
		     (yyval.id) = 0;
		   }
               }
    break;

  case 256:

/* Line 1464 of yacc.c  */
#line 4738 "parser.y"
    { (yyval.id) = "extern"; }
    break;

  case 257:

/* Line 1464 of yacc.c  */
#line 4739 "parser.y"
    { (yyval.id) = (yyvsp[(1) - (1)].id); }
    break;

  case 258:

/* Line 1464 of yacc.c  */
#line 4740 "parser.y"
    { (yyval.id) = "thread_local"; }
    break;

  case 259:

/* Line 1464 of yacc.c  */
#line 4741 "parser.y"
    { (yyval.id) = "typedef"; }
    break;

  case 260:

/* Line 1464 of yacc.c  */
#line 4742 "parser.y"
    { (yyval.id) = "static"; }
    break;

  case 261:

/* Line 1464 of yacc.c  */
#line 4743 "parser.y"
    { (yyval.id) = "typedef"; }
    break;

  case 262:

/* Line 1464 of yacc.c  */
#line 4744 "parser.y"
    { (yyval.id) = "virtual"; }
    break;

  case 263:

/* Line 1464 of yacc.c  */
#line 4745 "parser.y"
    { (yyval.id) = "friend"; }
    break;

  case 264:

/* Line 1464 of yacc.c  */
#line 4746 "parser.y"
    { (yyval.id) = "explicit"; }
    break;

  case 265:

/* Line 1464 of yacc.c  */
#line 4747 "parser.y"
    { (yyval.id) = "constexpr"; }
    break;

  case 266:

/* Line 1464 of yacc.c  */
#line 4748 "parser.y"
    { (yyval.id) = "explicit constexpr"; }
    break;

  case 267:

/* Line 1464 of yacc.c  */
#line 4749 "parser.y"
    { (yyval.id) = "explicit constexpr"; }
    break;

  case 268:

/* Line 1464 of yacc.c  */
#line 4750 "parser.y"
    { (yyval.id) = "static constexpr"; }
    break;

  case 269:

/* Line 1464 of yacc.c  */
#line 4751 "parser.y"
    { (yyval.id) = "static constexpr"; }
    break;

  case 270:

/* Line 1464 of yacc.c  */
#line 4752 "parser.y"
    { (yyval.id) = "thread_local"; }
    break;

  case 271:

/* Line 1464 of yacc.c  */
#line 4753 "parser.y"
    { (yyval.id) = "static thread_local"; }
    break;

  case 272:

/* Line 1464 of yacc.c  */
#line 4754 "parser.y"
    { (yyval.id) = "static thread_local"; }
    break;

  case 273:

/* Line 1464 of yacc.c  */
#line 4755 "parser.y"
    { (yyval.id) = "extern thread_local"; }
    break;

  case 274:

/* Line 1464 of yacc.c  */
#line 4756 "parser.y"
    { (yyval.id) = "extern thread_local"; }
    break;

  case 275:

/* Line 1464 of yacc.c  */
#line 4757 "parser.y"
    { (yyval.id) = 0; }
    break;

  case 276:

/* Line 1464 of yacc.c  */
#line 4764 "parser.y"
    {
                 Parm *p;
		 (yyval.pl) = (yyvsp[(1) - (1)].pl);
		 p = (yyvsp[(1) - (1)].pl);
                 while (p) {
		   Replace(Getattr(p,"type"),"typename ", "", DOH_REPLACE_ANY);
		   p = nextSibling(p);
                 }
               }
    break;

  case 277:

/* Line 1464 of yacc.c  */
#line 4775 "parser.y"
    {
                  set_nextSibling((yyvsp[(1) - (2)].p),(yyvsp[(2) - (2)].pl));
                  (yyval.pl) = (yyvsp[(1) - (2)].p);
		}
    break;

  case 278:

/* Line 1464 of yacc.c  */
#line 4779 "parser.y"
    { (yyval.pl) = 0; }
    break;

  case 279:

/* Line 1464 of yacc.c  */
#line 4782 "parser.y"
    {
                 set_nextSibling((yyvsp[(2) - (3)].p),(yyvsp[(3) - (3)].pl));
		 (yyval.pl) = (yyvsp[(2) - (3)].p);
                }
    break;

  case 280:

/* Line 1464 of yacc.c  */
#line 4786 "parser.y"
    { (yyval.pl) = 0; }
    break;

  case 281:

/* Line 1464 of yacc.c  */
#line 4790 "parser.y"
    {
                   SwigType_push((yyvsp[(1) - (2)].type),(yyvsp[(2) - (2)].decl).type);
		   (yyval.p) = NewParmWithoutFileLineInfo((yyvsp[(1) - (2)].type),(yyvsp[(2) - (2)].decl).id);
		   Setfile((yyval.p),cparse_file);
		   Setline((yyval.p),cparse_line);
		   if ((yyvsp[(2) - (2)].decl).defarg) {
		     Setattr((yyval.p),"value",(yyvsp[(2) - (2)].decl).defarg);
		   }
		}
    break;

  case 282:

/* Line 1464 of yacc.c  */
#line 4800 "parser.y"
    {
                  (yyval.p) = NewParmWithoutFileLineInfo(NewStringf("template<class> %s %s", (yyvsp[(5) - (7)].id),(yyvsp[(6) - (7)].str)), 0);
		  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
                  if ((yyvsp[(7) - (7)].dtype).val) {
                    Setattr((yyval.p),"value",(yyvsp[(7) - (7)].dtype).val);
                  }
                }
    break;

  case 283:

/* Line 1464 of yacc.c  */
#line 4808 "parser.y"
    {
		  SwigType *t = NewString("v(...)");
		  (yyval.p) = NewParmWithoutFileLineInfo(t, 0);
		  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
		}
    break;

  case 284:

/* Line 1464 of yacc.c  */
#line 4816 "parser.y"
    {
                 Parm *p;
		 (yyval.p) = (yyvsp[(1) - (1)].p);
		 p = (yyvsp[(1) - (1)].p);
                 while (p) {
		   if (Getattr(p,"type")) {
		     Replace(Getattr(p,"type"),"typename ", "", DOH_REPLACE_ANY);
		   }
		   p = nextSibling(p);
                 }
               }
    break;

  case 285:

/* Line 1464 of yacc.c  */
#line 4829 "parser.y"
    {
                  set_nextSibling((yyvsp[(1) - (2)].p),(yyvsp[(2) - (2)].p));
                  (yyval.p) = (yyvsp[(1) - (2)].p);
		}
    break;

  case 286:

/* Line 1464 of yacc.c  */
#line 4833 "parser.y"
    { (yyval.p) = 0; }
    break;

  case 287:

/* Line 1464 of yacc.c  */
#line 4836 "parser.y"
    {
                 set_nextSibling((yyvsp[(2) - (3)].p),(yyvsp[(3) - (3)].p));
		 (yyval.p) = (yyvsp[(2) - (3)].p);
                }
    break;

  case 288:

/* Line 1464 of yacc.c  */
#line 4840 "parser.y"
    { (yyval.p) = 0; }
    break;

  case 289:

/* Line 1464 of yacc.c  */
#line 4844 "parser.y"
    {
		  (yyval.p) = (yyvsp[(1) - (1)].p);
		  {
		    /* We need to make a possible adjustment for integer parameters. */
		    SwigType *type;
		    Node     *n = 0;

		    while (!n) {
		      type = Getattr((yyvsp[(1) - (1)].p),"type");
		      n = Swig_symbol_clookup(type,0);     /* See if we can find a node that matches the typename */
		      if ((n) && (Strcmp(nodeType(n),"cdecl") == 0)) {
			SwigType *decl = Getattr(n,"decl");
			if (!SwigType_isfunction(decl)) {
			  String *value = Getattr(n,"value");
			  if (value) {
			    String *v = Copy(value);
			    Setattr((yyvsp[(1) - (1)].p),"type",v);
			    Delete(v);
			    n = 0;
			  }
			}
		      } else {
			break;
		      }
		    }
		  }

               }
    break;

  case 290:

/* Line 1464 of yacc.c  */
#line 4872 "parser.y"
    {
                  (yyval.p) = NewParmWithoutFileLineInfo(0,0);
                  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
		  Setattr((yyval.p),"value",(yyvsp[(1) - (1)].dtype).val);
               }
    break;

  case 291:

/* Line 1464 of yacc.c  */
#line 4880 "parser.y"
    { 
                  (yyval.dtype) = (yyvsp[(2) - (2)].dtype); 
		  if ((yyvsp[(2) - (2)].dtype).type == T_ERROR) {
		    Swig_warning(WARN_PARSE_BAD_DEFAULT,cparse_file, cparse_line, "Can't set default argument (ignored)\n");
		    (yyval.dtype).val = 0;
		    (yyval.dtype).rawval = 0;
		    (yyval.dtype).bitfield = 0;
		    (yyval.dtype).throws = 0;
		    (yyval.dtype).throwf = 0;
		    (yyval.dtype).nexcept = 0;
		  }
               }
    break;

  case 292:

/* Line 1464 of yacc.c  */
#line 4892 "parser.y"
    { 
		  (yyval.dtype) = (yyvsp[(2) - (5)].dtype);
		  if ((yyvsp[(2) - (5)].dtype).type == T_ERROR) {
		    Swig_warning(WARN_PARSE_BAD_DEFAULT,cparse_file, cparse_line, "Can't set default argument (ignored)\n");
		    (yyval.dtype) = (yyvsp[(2) - (5)].dtype);
		    (yyval.dtype).val = 0;
		    (yyval.dtype).rawval = 0;
		    (yyval.dtype).bitfield = 0;
		    (yyval.dtype).throws = 0;
		    (yyval.dtype).throwf = 0;
		    (yyval.dtype).nexcept = 0;
		  } else {
		    (yyval.dtype).val = NewStringf("%s[%s]",(yyvsp[(2) - (5)].dtype).val,(yyvsp[(4) - (5)].dtype).val); 
		  }		  
               }
    break;

  case 293:

/* Line 1464 of yacc.c  */
#line 4907 "parser.y"
    {
		 skip_balanced('{','}');
		 (yyval.dtype).val = NewString(scanner_ccode);
		 (yyval.dtype).rawval = 0;
                 (yyval.dtype).type = T_INT;
		 (yyval.dtype).bitfield = 0;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
		 (yyval.dtype).nexcept = 0;
	       }
    break;

  case 294:

/* Line 1464 of yacc.c  */
#line 4917 "parser.y"
    { 
		 (yyval.dtype).val = 0;
		 (yyval.dtype).rawval = 0;
		 (yyval.dtype).type = 0;
		 (yyval.dtype).bitfield = (yyvsp[(2) - (2)].dtype).val;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
		 (yyval.dtype).nexcept = 0;
	       }
    break;

  case 295:

/* Line 1464 of yacc.c  */
#line 4926 "parser.y"
    {
                 (yyval.dtype).val = 0;
                 (yyval.dtype).rawval = 0;
                 (yyval.dtype).type = T_INT;
		 (yyval.dtype).bitfield = 0;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
		 (yyval.dtype).nexcept = 0;
               }
    break;

  case 296:

/* Line 1464 of yacc.c  */
#line 4937 "parser.y"
    {
                 (yyval.decl) = (yyvsp[(1) - (2)].decl);
		 (yyval.decl).defarg = (yyvsp[(2) - (2)].dtype).rawval ? (yyvsp[(2) - (2)].dtype).rawval : (yyvsp[(2) - (2)].dtype).val;
            }
    break;

  case 297:

/* Line 1464 of yacc.c  */
#line 4941 "parser.y"
    {
              (yyval.decl) = (yyvsp[(1) - (2)].decl);
	      (yyval.decl).defarg = (yyvsp[(2) - (2)].dtype).rawval ? (yyvsp[(2) - (2)].dtype).rawval : (yyvsp[(2) - (2)].dtype).val;
            }
    break;

  case 298:

/* Line 1464 of yacc.c  */
#line 4945 "parser.y"
    {
   	      (yyval.decl).type = 0;
              (yyval.decl).id = 0;
	      (yyval.decl).defarg = (yyvsp[(1) - (1)].dtype).rawval ? (yyvsp[(1) - (1)].dtype).rawval : (yyvsp[(1) - (1)].dtype).val;
            }
    break;

  case 299:

/* Line 1464 of yacc.c  */
#line 4952 "parser.y"
    {
                 (yyval.decl) = (yyvsp[(1) - (1)].decl);
		 if (SwigType_isfunction((yyvsp[(1) - (1)].decl).type)) {
		   Delete(SwigType_pop_function((yyvsp[(1) - (1)].decl).type));
		 } else if (SwigType_isarray((yyvsp[(1) - (1)].decl).type)) {
		   SwigType *ta = SwigType_pop_arrays((yyvsp[(1) - (1)].decl).type);
		   if (SwigType_isfunction((yyvsp[(1) - (1)].decl).type)) {
		     Delete(SwigType_pop_function((yyvsp[(1) - (1)].decl).type));
		   } else {
		     (yyval.decl).parms = 0;
		   }
		   SwigType_push((yyvsp[(1) - (1)].decl).type,ta);
		   Delete(ta);
		 } else {
		   (yyval.decl).parms = 0;
		 }
            }
    break;

  case 300:

/* Line 1464 of yacc.c  */
#line 4969 "parser.y"
    {
              (yyval.decl) = (yyvsp[(1) - (1)].decl);
	      if (SwigType_isfunction((yyvsp[(1) - (1)].decl).type)) {
		Delete(SwigType_pop_function((yyvsp[(1) - (1)].decl).type));
	      } else if (SwigType_isarray((yyvsp[(1) - (1)].decl).type)) {
		SwigType *ta = SwigType_pop_arrays((yyvsp[(1) - (1)].decl).type);
		if (SwigType_isfunction((yyvsp[(1) - (1)].decl).type)) {
		  Delete(SwigType_pop_function((yyvsp[(1) - (1)].decl).type));
		} else {
		  (yyval.decl).parms = 0;
		}
		SwigType_push((yyvsp[(1) - (1)].decl).type,ta);
		Delete(ta);
	      } else {
		(yyval.decl).parms = 0;
	      }
            }
    break;

  case 301:

/* Line 1464 of yacc.c  */
#line 4986 "parser.y"
    {
   	      (yyval.decl).type = 0;
              (yyval.decl).id = 0;
	      (yyval.decl).parms = 0;
	      }
    break;

  case 302:

/* Line 1464 of yacc.c  */
#line 4994 "parser.y"
    {
              (yyval.decl) = (yyvsp[(2) - (2)].decl);
	      if ((yyval.decl).type) {
		SwigType_push((yyvsp[(1) - (2)].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[(1) - (2)].type);
           }
    break;

  case 303:

/* Line 1464 of yacc.c  */
#line 5002 "parser.y"
    {
              (yyval.decl) = (yyvsp[(3) - (3)].decl);
	      SwigType_add_reference((yyvsp[(1) - (3)].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[(1) - (3)].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[(1) - (3)].type);
           }
    break;

  case 304:

/* Line 1464 of yacc.c  */
#line 5011 "parser.y"
    {
              (yyval.decl) = (yyvsp[(3) - (3)].decl);
	      SwigType_add_rvalue_reference((yyvsp[(1) - (3)].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[(1) - (3)].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[(1) - (3)].type);
           }
    break;

  case 305:

/* Line 1464 of yacc.c  */
#line 5020 "parser.y"
    {
              (yyval.decl) = (yyvsp[(1) - (1)].decl);
	      if (!(yyval.decl).type) (yyval.decl).type = NewStringEmpty();
           }
    break;

  case 306:

/* Line 1464 of yacc.c  */
#line 5024 "parser.y"
    {
	     (yyval.decl) = (yyvsp[(2) - (2)].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_reference((yyval.decl).type);
	     if ((yyvsp[(2) - (2)].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[(2) - (2)].decl).type);
	       Delete((yyvsp[(2) - (2)].decl).type);
	     }
           }
    break;

  case 307:

/* Line 1464 of yacc.c  */
#line 5033 "parser.y"
    {
	     /* Introduced in C++11, move operator && */
             /* Adds one S/R conflict */
	     (yyval.decl) = (yyvsp[(2) - (2)].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_rvalue_reference((yyval.decl).type);
	     if ((yyvsp[(2) - (2)].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[(2) - (2)].decl).type);
	       Delete((yyvsp[(2) - (2)].decl).type);
	     }
           }
    break;

  case 308:

/* Line 1464 of yacc.c  */
#line 5044 "parser.y"
    { 
	     SwigType *t = NewStringEmpty();

	     (yyval.decl) = (yyvsp[(3) - (3)].decl);
	     SwigType_add_memberpointer(t,(yyvsp[(1) - (3)].str));
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = t;
	     }
    break;

  case 309:

/* Line 1464 of yacc.c  */
#line 5055 "parser.y"
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[(4) - (4)].decl);
	     SwigType_add_memberpointer(t,(yyvsp[(2) - (4)].str));
	     SwigType_push((yyvsp[(1) - (4)].type),t);
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[(1) - (4)].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[(1) - (4)].type);
	     Delete(t);
	   }
    break;

  case 310:

/* Line 1464 of yacc.c  */
#line 5067 "parser.y"
    { 
	     (yyval.decl) = (yyvsp[(5) - (5)].decl);
	     SwigType_add_memberpointer((yyvsp[(1) - (5)].type),(yyvsp[(2) - (5)].str));
	     SwigType_add_reference((yyvsp[(1) - (5)].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[(1) - (5)].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[(1) - (5)].type);
	   }
    break;

  case 311:

/* Line 1464 of yacc.c  */
#line 5077 "parser.y"
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[(4) - (4)].decl);
	     SwigType_add_memberpointer(t,(yyvsp[(1) - (4)].str));
	     SwigType_add_reference(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
    break;

  case 312:

/* Line 1464 of yacc.c  */
#line 5091 "parser.y"
    {
              (yyval.decl) = (yyvsp[(5) - (5)].decl);
	      if ((yyval.decl).type) {
		SwigType_push((yyvsp[(1) - (5)].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[(1) - (5)].type);
           }
    break;

  case 313:

/* Line 1464 of yacc.c  */
#line 5099 "parser.y"
    {
              (yyval.decl) = (yyvsp[(6) - (6)].decl);
	      SwigType_add_reference((yyvsp[(1) - (6)].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[(1) - (6)].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[(1) - (6)].type);
           }
    break;

  case 314:

/* Line 1464 of yacc.c  */
#line 5108 "parser.y"
    {
              (yyval.decl) = (yyvsp[(6) - (6)].decl);
	      SwigType_add_rvalue_reference((yyvsp[(1) - (6)].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[(1) - (6)].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[(1) - (6)].type);
           }
    break;

  case 315:

/* Line 1464 of yacc.c  */
#line 5117 "parser.y"
    {
              (yyval.decl) = (yyvsp[(4) - (4)].decl);
	      if (!(yyval.decl).type) (yyval.decl).type = NewStringEmpty();
           }
    break;

  case 316:

/* Line 1464 of yacc.c  */
#line 5121 "parser.y"
    {
	     (yyval.decl) = (yyvsp[(5) - (5)].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_reference((yyval.decl).type);
	     if ((yyvsp[(5) - (5)].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[(5) - (5)].decl).type);
	       Delete((yyvsp[(5) - (5)].decl).type);
	     }
           }
    break;

  case 317:

/* Line 1464 of yacc.c  */
#line 5130 "parser.y"
    {
	     /* Introduced in C++11, move operator && */
             /* Adds one S/R conflict */
	     (yyval.decl) = (yyvsp[(5) - (5)].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_rvalue_reference((yyval.decl).type);
	     if ((yyvsp[(5) - (5)].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[(5) - (5)].decl).type);
	       Delete((yyvsp[(5) - (5)].decl).type);
	     }
           }
    break;

  case 318:

/* Line 1464 of yacc.c  */
#line 5141 "parser.y"
    { 
	     SwigType *t = NewStringEmpty();

	     (yyval.decl) = (yyvsp[(6) - (6)].decl);
	     SwigType_add_memberpointer(t,(yyvsp[(1) - (6)].str));
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = t;
	     }
    break;

  case 319:

/* Line 1464 of yacc.c  */
#line 5152 "parser.y"
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[(7) - (7)].decl);
	     SwigType_add_memberpointer(t,(yyvsp[(2) - (7)].str));
	     SwigType_push((yyvsp[(1) - (7)].type),t);
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[(1) - (7)].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[(1) - (7)].type);
	     Delete(t);
	   }
    break;

  case 320:

/* Line 1464 of yacc.c  */
#line 5164 "parser.y"
    { 
	     (yyval.decl) = (yyvsp[(8) - (8)].decl);
	     SwigType_add_memberpointer((yyvsp[(1) - (8)].type),(yyvsp[(2) - (8)].str));
	     SwigType_add_reference((yyvsp[(1) - (8)].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[(1) - (8)].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[(1) - (8)].type);
	   }
    break;

  case 321:

/* Line 1464 of yacc.c  */
#line 5174 "parser.y"
    { 
	     (yyval.decl) = (yyvsp[(8) - (8)].decl);
	     SwigType_add_memberpointer((yyvsp[(1) - (8)].type),(yyvsp[(2) - (8)].str));
	     SwigType_add_rvalue_reference((yyvsp[(1) - (8)].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[(1) - (8)].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[(1) - (8)].type);
	   }
    break;

  case 322:

/* Line 1464 of yacc.c  */
#line 5184 "parser.y"
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[(7) - (7)].decl);
	     SwigType_add_memberpointer(t,(yyvsp[(1) - (7)].str));
	     SwigType_add_reference(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
    break;

  case 323:

/* Line 1464 of yacc.c  */
#line 5195 "parser.y"
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[(7) - (7)].decl);
	     SwigType_add_memberpointer(t,(yyvsp[(1) - (7)].str));
	     SwigType_add_rvalue_reference(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
    break;

  case 324:

/* Line 1464 of yacc.c  */
#line 5208 "parser.y"
    {
                /* Note: This is non-standard C.  Template declarator is allowed to follow an identifier */
                 (yyval.decl).id = Char((yyvsp[(1) - (1)].str));
		 (yyval.decl).type = 0;
		 (yyval.decl).parms = 0;
		 (yyval.decl).have_parms = 0;
                  }
    break;

  case 325:

/* Line 1464 of yacc.c  */
#line 5215 "parser.y"
    {
                  (yyval.decl).id = Char(NewStringf("~%s",(yyvsp[(2) - (2)].str)));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
    break;

  case 326:

/* Line 1464 of yacc.c  */
#line 5223 "parser.y"
    {
                  (yyval.decl).id = Char((yyvsp[(2) - (3)].str));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
    break;

  case 327:

/* Line 1464 of yacc.c  */
#line 5239 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(3) - (4)].decl);
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[(2) - (4)].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[(2) - (4)].type);
                  }
    break;

  case 328:

/* Line 1464 of yacc.c  */
#line 5247 "parser.y"
    {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(4) - (5)].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t,(yyvsp[(2) - (5)].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		    }
    break;

  case 329:

/* Line 1464 of yacc.c  */
#line 5258 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (3)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 330:

/* Line 1464 of yacc.c  */
#line 5269 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[(3) - (4)].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 331:

/* Line 1464 of yacc.c  */
#line 5280 "parser.y"
    {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[(3) - (4)].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[(3) - (4)].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		  }
    break;

  case 332:

/* Line 1464 of yacc.c  */
#line 5299 "parser.y"
    {
                /* Note: This is non-standard C.  Template declarator is allowed to follow an identifier */
                 (yyval.decl).id = Char((yyvsp[(1) - (1)].str));
		 (yyval.decl).type = 0;
		 (yyval.decl).parms = 0;
		 (yyval.decl).have_parms = 0;
                  }
    break;

  case 333:

/* Line 1464 of yacc.c  */
#line 5307 "parser.y"
    {
                  (yyval.decl).id = Char(NewStringf("~%s",(yyvsp[(2) - (2)].str)));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
    break;

  case 334:

/* Line 1464 of yacc.c  */
#line 5324 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(3) - (4)].decl);
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[(2) - (4)].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[(2) - (4)].type);
                  }
    break;

  case 335:

/* Line 1464 of yacc.c  */
#line 5332 "parser.y"
    {
                    (yyval.decl) = (yyvsp[(3) - (4)].decl);
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = NewStringEmpty();
		    }
		    SwigType_add_reference((yyval.decl).type);
                  }
    break;

  case 336:

/* Line 1464 of yacc.c  */
#line 5339 "parser.y"
    {
                    (yyval.decl) = (yyvsp[(3) - (4)].decl);
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = NewStringEmpty();
		    }
		    SwigType_add_rvalue_reference((yyval.decl).type);
                  }
    break;

  case 337:

/* Line 1464 of yacc.c  */
#line 5346 "parser.y"
    {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(4) - (5)].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t,(yyvsp[(2) - (5)].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		    }
    break;

  case 338:

/* Line 1464 of yacc.c  */
#line 5357 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (3)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 339:

/* Line 1464 of yacc.c  */
#line 5368 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[(3) - (4)].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 340:

/* Line 1464 of yacc.c  */
#line 5379 "parser.y"
    {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[(3) - (4)].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[(3) - (4)].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
                 }
    break;

  case 341:

/* Line 1464 of yacc.c  */
#line 5399 "parser.y"
    {
		    SwigType *t;
                    Append((yyvsp[(1) - (5)].str), " "); /* intervening space is mandatory */
                    Append((yyvsp[(1) - (5)].str), Char((yyvsp[(2) - (5)].id)));
		    (yyval.decl).id = Char((yyvsp[(1) - (5)].str));
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[(4) - (5)].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[(4) - (5)].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		  }
    break;

  case 342:

/* Line 1464 of yacc.c  */
#line 5420 "parser.y"
    {
		    (yyval.decl).type = (yyvsp[(1) - (1)].type);
                    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
                  }
    break;

  case 343:

/* Line 1464 of yacc.c  */
#line 5426 "parser.y"
    { 
                     (yyval.decl) = (yyvsp[(2) - (2)].decl);
                     SwigType_push((yyvsp[(1) - (2)].type),(yyvsp[(2) - (2)].decl).type);
		     (yyval.decl).type = (yyvsp[(1) - (2)].type);
		     Delete((yyvsp[(2) - (2)].decl).type);
                  }
    break;

  case 344:

/* Line 1464 of yacc.c  */
#line 5432 "parser.y"
    {
		    (yyval.decl).type = (yyvsp[(1) - (2)].type);
		    SwigType_add_reference((yyval.decl).type);
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		  }
    break;

  case 345:

/* Line 1464 of yacc.c  */
#line 5439 "parser.y"
    {
		    (yyval.decl).type = (yyvsp[(1) - (2)].type);
		    SwigType_add_rvalue_reference((yyval.decl).type);
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		  }
    break;

  case 346:

/* Line 1464 of yacc.c  */
#line 5446 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(3) - (3)].decl);
		    SwigType_add_reference((yyvsp[(1) - (3)].type));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[(1) - (3)].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[(1) - (3)].type);
                  }
    break;

  case 347:

/* Line 1464 of yacc.c  */
#line 5455 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(3) - (3)].decl);
		    SwigType_add_rvalue_reference((yyvsp[(1) - (3)].type));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[(1) - (3)].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[(1) - (3)].type);
                  }
    break;

  case 348:

/* Line 1464 of yacc.c  */
#line 5464 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(1) - (1)].decl);
                  }
    break;

  case 349:

/* Line 1464 of yacc.c  */
#line 5467 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(2) - (2)].decl);
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_reference((yyval.decl).type);
		    if ((yyvsp[(2) - (2)].decl).type) {
		      SwigType_push((yyval.decl).type,(yyvsp[(2) - (2)].decl).type);
		      Delete((yyvsp[(2) - (2)].decl).type);
		    }
                  }
    break;

  case 350:

/* Line 1464 of yacc.c  */
#line 5476 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(2) - (2)].decl);
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_rvalue_reference((yyval.decl).type);
		    if ((yyvsp[(2) - (2)].decl).type) {
		      SwigType_push((yyval.decl).type,(yyvsp[(2) - (2)].decl).type);
		      Delete((yyvsp[(2) - (2)].decl).type);
		    }
                  }
    break;

  case 351:

/* Line 1464 of yacc.c  */
#line 5485 "parser.y"
    {
                    (yyval.decl).id = 0;
                    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
                    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_reference((yyval.decl).type);
                  }
    break;

  case 352:

/* Line 1464 of yacc.c  */
#line 5492 "parser.y"
    {
                    (yyval.decl).id = 0;
                    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
                    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_rvalue_reference((yyval.decl).type);
                  }
    break;

  case 353:

/* Line 1464 of yacc.c  */
#line 5499 "parser.y"
    { 
		    (yyval.decl).type = NewStringEmpty();
                    SwigType_add_memberpointer((yyval.decl).type,(yyvsp[(1) - (2)].str));
                    (yyval.decl).id = 0;
                    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
      	          }
    break;

  case 354:

/* Line 1464 of yacc.c  */
#line 5506 "parser.y"
    { 
		    SwigType *t = NewStringEmpty();
                    (yyval.decl).type = (yyvsp[(1) - (3)].type);
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_memberpointer(t,(yyvsp[(2) - (3)].str));
		    SwigType_push((yyval.decl).type,t);
		    Delete(t);
                  }
    break;

  case 355:

/* Line 1464 of yacc.c  */
#line 5516 "parser.y"
    { 
		    (yyval.decl) = (yyvsp[(4) - (4)].decl);
		    SwigType_add_memberpointer((yyvsp[(1) - (4)].type),(yyvsp[(2) - (4)].str));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[(1) - (4)].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[(1) - (4)].type);
                  }
    break;

  case 356:

/* Line 1464 of yacc.c  */
#line 5527 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (3)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 357:

/* Line 1464 of yacc.c  */
#line 5538 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[(3) - (4)].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 358:

/* Line 1464 of yacc.c  */
#line 5549 "parser.y"
    { 
		    (yyval.decl).type = NewStringEmpty();
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_array((yyval.decl).type,"");
                  }
    break;

  case 359:

/* Line 1464 of yacc.c  */
#line 5556 "parser.y"
    { 
		    (yyval.decl).type = NewStringEmpty();
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_array((yyval.decl).type,(yyvsp[(2) - (3)].dtype).val);
		  }
    break;

  case 360:

/* Line 1464 of yacc.c  */
#line 5563 "parser.y"
    {
                    (yyval.decl) = (yyvsp[(2) - (3)].decl);
		  }
    break;

  case 361:

/* Line 1464 of yacc.c  */
#line 5566 "parser.y"
    {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
                    SwigType_add_function(t,(yyvsp[(3) - (4)].pl));
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[(3) - (4)].pl);
		      (yyval.decl).have_parms = 1;
		    }
		  }
    break;

  case 362:

/* Line 1464 of yacc.c  */
#line 5583 "parser.y"
    {
                    (yyval.decl).type = NewStringEmpty();
                    SwigType_add_function((yyval.decl).type,(yyvsp[(2) - (3)].pl));
		    (yyval.decl).parms = (yyvsp[(2) - (3)].pl);
		    (yyval.decl).have_parms = 1;
		    (yyval.decl).id = 0;
                  }
    break;

  case 363:

/* Line 1464 of yacc.c  */
#line 5593 "parser.y"
    { 
             (yyval.type) = NewStringEmpty();
             SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[(2) - (3)].str));
	     SwigType_push((yyval.type),(yyvsp[(3) - (3)].type));
	     Delete((yyvsp[(3) - (3)].type));
           }
    break;

  case 364:

/* Line 1464 of yacc.c  */
#line 5600 "parser.y"
    {
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[(2) - (2)].type));
	     Delete((yyvsp[(2) - (2)].type));
	   }
    break;

  case 365:

/* Line 1464 of yacc.c  */
#line 5606 "parser.y"
    { 
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[(2) - (2)].str));
           }
    break;

  case 366:

/* Line 1464 of yacc.c  */
#line 5611 "parser.y"
    {
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
           }
    break;

  case 367:

/* Line 1464 of yacc.c  */
#line 5617 "parser.y"
    {
	          (yyval.str) = NewStringEmpty();
	          if ((yyvsp[(1) - (1)].id)) SwigType_add_qualifier((yyval.str),(yyvsp[(1) - (1)].id));
               }
    break;

  case 368:

/* Line 1464 of yacc.c  */
#line 5621 "parser.y"
    {
		  (yyval.str) = (yyvsp[(2) - (2)].str);
	          if ((yyvsp[(1) - (2)].id)) SwigType_add_qualifier((yyval.str),(yyvsp[(1) - (2)].id));
               }
    break;

  case 369:

/* Line 1464 of yacc.c  */
#line 5627 "parser.y"
    { (yyval.id) = "const"; }
    break;

  case 370:

/* Line 1464 of yacc.c  */
#line 5628 "parser.y"
    { (yyval.id) = "volatile"; }
    break;

  case 371:

/* Line 1464 of yacc.c  */
#line 5629 "parser.y"
    { (yyval.id) = 0; }
    break;

  case 372:

/* Line 1464 of yacc.c  */
#line 5635 "parser.y"
    {
                   (yyval.type) = (yyvsp[(1) - (1)].type);
                   Replace((yyval.type),"typename ","", DOH_REPLACE_ANY);
                }
    break;

  case 373:

/* Line 1464 of yacc.c  */
#line 5641 "parser.y"
    {
                   (yyval.type) = (yyvsp[(2) - (2)].type);
	           SwigType_push((yyval.type),(yyvsp[(1) - (2)].str));
               }
    break;

  case 374:

/* Line 1464 of yacc.c  */
#line 5645 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type); }
    break;

  case 375:

/* Line 1464 of yacc.c  */
#line 5646 "parser.y"
    {
		  (yyval.type) = (yyvsp[(1) - (2)].type);
	          SwigType_push((yyval.type),(yyvsp[(2) - (2)].str));
	       }
    break;

  case 376:

/* Line 1464 of yacc.c  */
#line 5650 "parser.y"
    {
		  (yyval.type) = (yyvsp[(2) - (3)].type);
	          SwigType_push((yyval.type),(yyvsp[(3) - (3)].str));
	          SwigType_push((yyval.type),(yyvsp[(1) - (3)].str));
	       }
    break;

  case 377:

/* Line 1464 of yacc.c  */
#line 5657 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type);
                  /* Printf(stdout,"primitive = '%s'\n", $$);*/
               }
    break;

  case 378:

/* Line 1464 of yacc.c  */
#line 5660 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type); }
    break;

  case 379:

/* Line 1464 of yacc.c  */
#line 5661 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type); }
    break;

  case 380:

/* Line 1464 of yacc.c  */
#line 5665 "parser.y"
    { (yyval.type) = NewStringf("enum %s", (yyvsp[(2) - (2)].str)); }
    break;

  case 381:

/* Line 1464 of yacc.c  */
#line 5666 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type); }
    break;

  case 382:

/* Line 1464 of yacc.c  */
#line 5668 "parser.y"
    {
		  (yyval.type) = (yyvsp[(1) - (1)].str);
               }
    break;

  case 383:

/* Line 1464 of yacc.c  */
#line 5671 "parser.y"
    { 
		 (yyval.type) = NewStringf("%s %s", (yyvsp[(1) - (2)].id), (yyvsp[(2) - (2)].str));
               }
    break;

  case 384:

/* Line 1464 of yacc.c  */
#line 5674 "parser.y"
    {
                 (yyval.type) = (yyvsp[(1) - (1)].type);
               }
    break;

  case 385:

/* Line 1464 of yacc.c  */
#line 5679 "parser.y"
    {
                 Node *n = Swig_symbol_clookup((yyvsp[(3) - (4)].str),0);
                 if (!n) {
		   Swig_error(cparse_file, cparse_line, "Identifier %s not defined.\n", (yyvsp[(3) - (4)].str));
                   (yyval.type) = (yyvsp[(3) - (4)].str);
                 } else {
                   (yyval.type) = Getattr(n, "type");
                 }
               }
    break;

  case 386:

/* Line 1464 of yacc.c  */
#line 5690 "parser.y"
    {
		 if (!(yyvsp[(1) - (1)].ptype).type) (yyvsp[(1) - (1)].ptype).type = NewString("int");
		 if ((yyvsp[(1) - (1)].ptype).us) {
		   (yyval.type) = NewStringf("%s %s", (yyvsp[(1) - (1)].ptype).us, (yyvsp[(1) - (1)].ptype).type);
		   Delete((yyvsp[(1) - (1)].ptype).us);
                   Delete((yyvsp[(1) - (1)].ptype).type);
		 } else {
                   (yyval.type) = (yyvsp[(1) - (1)].ptype).type;
		 }
		 if (Cmp((yyval.type),"signed int") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("int");
                 } else if (Cmp((yyval.type),"signed long") == 0) {
		   Delete((yyval.type));
                   (yyval.type) = NewString("long");
                 } else if (Cmp((yyval.type),"signed short") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("short");
		 } else if (Cmp((yyval.type),"signed long long") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("long long");
		 }
               }
    break;

  case 387:

/* Line 1464 of yacc.c  */
#line 5715 "parser.y"
    { 
                 (yyval.ptype) = (yyvsp[(1) - (1)].ptype);
               }
    break;

  case 388:

/* Line 1464 of yacc.c  */
#line 5718 "parser.y"
    {
                    if ((yyvsp[(1) - (2)].ptype).us && (yyvsp[(2) - (2)].ptype).us) {
		      Swig_error(cparse_file, cparse_line, "Extra %s specifier.\n", (yyvsp[(2) - (2)].ptype).us);
		    }
                    (yyval.ptype) = (yyvsp[(2) - (2)].ptype);
                    if ((yyvsp[(1) - (2)].ptype).us) (yyval.ptype).us = (yyvsp[(1) - (2)].ptype).us;
		    if ((yyvsp[(1) - (2)].ptype).type) {
		      if (!(yyvsp[(2) - (2)].ptype).type) (yyval.ptype).type = (yyvsp[(1) - (2)].ptype).type;
		      else {
			int err = 0;
			if ((Cmp((yyvsp[(1) - (2)].ptype).type,"long") == 0)) {
			  if ((Cmp((yyvsp[(2) - (2)].ptype).type,"long") == 0) || (Strncmp((yyvsp[(2) - (2)].ptype).type,"double",6) == 0)) {
			    (yyval.ptype).type = NewStringf("long %s", (yyvsp[(2) - (2)].ptype).type);
			  } else if (Cmp((yyvsp[(2) - (2)].ptype).type,"int") == 0) {
			    (yyval.ptype).type = (yyvsp[(1) - (2)].ptype).type;
			  } else {
			    err = 1;
			  }
			} else if ((Cmp((yyvsp[(1) - (2)].ptype).type,"short")) == 0) {
			  if (Cmp((yyvsp[(2) - (2)].ptype).type,"int") == 0) {
			    (yyval.ptype).type = (yyvsp[(1) - (2)].ptype).type;
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[(1) - (2)].ptype).type,"int") == 0) {
			  (yyval.ptype).type = (yyvsp[(2) - (2)].ptype).type;
			} else if (Cmp((yyvsp[(1) - (2)].ptype).type,"double") == 0) {
			  if (Cmp((yyvsp[(2) - (2)].ptype).type,"long") == 0) {
			    (yyval.ptype).type = NewString("long double");
			  } else if (Cmp((yyvsp[(2) - (2)].ptype).type,"complex") == 0) {
			    (yyval.ptype).type = NewString("double complex");
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[(1) - (2)].ptype).type,"float") == 0) {
			  if (Cmp((yyvsp[(2) - (2)].ptype).type,"complex") == 0) {
			    (yyval.ptype).type = NewString("float complex");
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[(1) - (2)].ptype).type,"complex") == 0) {
			  (yyval.ptype).type = NewStringf("%s complex", (yyvsp[(2) - (2)].ptype).type);
			} else {
			  err = 1;
			}
			if (err) {
			  Swig_error(cparse_file, cparse_line, "Extra %s specifier.\n", (yyvsp[(1) - (2)].ptype).type);
			}
		      }
		    }
               }
    break;

  case 389:

/* Line 1464 of yacc.c  */
#line 5772 "parser.y"
    { 
		    (yyval.ptype).type = NewString("int");
                    (yyval.ptype).us = 0;
               }
    break;

  case 390:

/* Line 1464 of yacc.c  */
#line 5776 "parser.y"
    { 
                    (yyval.ptype).type = NewString("short");
                    (yyval.ptype).us = 0;
                }
    break;

  case 391:

/* Line 1464 of yacc.c  */
#line 5780 "parser.y"
    { 
                    (yyval.ptype).type = NewString("long");
                    (yyval.ptype).us = 0;
                }
    break;

  case 392:

/* Line 1464 of yacc.c  */
#line 5784 "parser.y"
    { 
                    (yyval.ptype).type = NewString("char");
                    (yyval.ptype).us = 0;
                }
    break;

  case 393:

/* Line 1464 of yacc.c  */
#line 5788 "parser.y"
    { 
                    (yyval.ptype).type = NewString("wchar_t");
                    (yyval.ptype).us = 0;
                }
    break;

  case 394:

/* Line 1464 of yacc.c  */
#line 5792 "parser.y"
    { 
                    (yyval.ptype).type = NewString("float");
                    (yyval.ptype).us = 0;
                }
    break;

  case 395:

/* Line 1464 of yacc.c  */
#line 5796 "parser.y"
    { 
                    (yyval.ptype).type = NewString("double");
                    (yyval.ptype).us = 0;
                }
    break;

  case 396:

/* Line 1464 of yacc.c  */
#line 5800 "parser.y"
    { 
                    (yyval.ptype).us = NewString("signed");
                    (yyval.ptype).type = 0;
                }
    break;

  case 397:

/* Line 1464 of yacc.c  */
#line 5804 "parser.y"
    { 
                    (yyval.ptype).us = NewString("unsigned");
                    (yyval.ptype).type = 0;
                }
    break;

  case 398:

/* Line 1464 of yacc.c  */
#line 5808 "parser.y"
    { 
                    (yyval.ptype).type = NewString("complex");
                    (yyval.ptype).us = 0;
                }
    break;

  case 399:

/* Line 1464 of yacc.c  */
#line 5812 "parser.y"
    { 
                    (yyval.ptype).type = NewString("__int8");
                    (yyval.ptype).us = 0;
                }
    break;

  case 400:

/* Line 1464 of yacc.c  */
#line 5816 "parser.y"
    { 
                    (yyval.ptype).type = NewString("__int16");
                    (yyval.ptype).us = 0;
                }
    break;

  case 401:

/* Line 1464 of yacc.c  */
#line 5820 "parser.y"
    { 
                    (yyval.ptype).type = NewString("__int32");
                    (yyval.ptype).us = 0;
                }
    break;

  case 402:

/* Line 1464 of yacc.c  */
#line 5824 "parser.y"
    { 
                    (yyval.ptype).type = NewString("__int64");
                    (yyval.ptype).us = 0;
                }
    break;

  case 403:

/* Line 1464 of yacc.c  */
#line 5830 "parser.y"
    { /* scanner_check_typedef(); */ }
    break;

  case 404:

/* Line 1464 of yacc.c  */
#line 5830 "parser.y"
    {
                   (yyval.dtype) = (yyvsp[(2) - (2)].dtype);
		   if ((yyval.dtype).type == T_STRING) {
		     (yyval.dtype).rawval = NewStringf("\"%(escape)s\"",(yyval.dtype).val);
		   } else if ((yyval.dtype).type != T_CHAR && (yyval.dtype).type != T_WSTRING && (yyval.dtype).type != T_WCHAR) {
		     (yyval.dtype).rawval = NewStringf("%s", (yyval.dtype).val);
		   }
		   (yyval.dtype).qualifier = 0;
		   (yyval.dtype).bitfield = 0;
		   (yyval.dtype).throws = 0;
		   (yyval.dtype).throwf = 0;
		   (yyval.dtype).nexcept = 0;
		   scanner_ignore_typedef();
                }
    break;

  case 405:

/* Line 1464 of yacc.c  */
#line 5844 "parser.y"
    {
		  (yyval.dtype) = (yyvsp[(1) - (1)].dtype);
		}
    break;

  case 406:

/* Line 1464 of yacc.c  */
#line 5849 "parser.y"
    {
		  (yyval.dtype) = (yyvsp[(1) - (1)].dtype);
		}
    break;

  case 407:

/* Line 1464 of yacc.c  */
#line 5852 "parser.y"
    {
		  (yyval.dtype) = (yyvsp[(1) - (1)].dtype);
		}
    break;

  case 408:

/* Line 1464 of yacc.c  */
#line 5858 "parser.y"
    {
		  (yyval.dtype).val = NewString("delete");
		  (yyval.dtype).rawval = 0;
		  (yyval.dtype).type = T_STRING;
		  (yyval.dtype).qualifier = 0;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
		}
    break;

  case 409:

/* Line 1464 of yacc.c  */
#line 5871 "parser.y"
    {
		  (yyval.dtype).val = NewString("default");
		  (yyval.dtype).rawval = 0;
		  (yyval.dtype).type = T_STRING;
		  (yyval.dtype).qualifier = 0;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
		}
    break;

  case 410:

/* Line 1464 of yacc.c  */
#line 5885 "parser.y"
    { (yyval.id) = (yyvsp[(1) - (1)].id); }
    break;

  case 411:

/* Line 1464 of yacc.c  */
#line 5886 "parser.y"
    { (yyval.id) = (char *) 0;}
    break;

  case 412:

/* Line 1464 of yacc.c  */
#line 5889 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 413:

/* Line 1464 of yacc.c  */
#line 5890 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 414:

/* Line 1464 of yacc.c  */
#line 5894 "parser.y"
    {
		 Node *leftSibling = Getattr((yyvsp[(1) - (5)].node),"_last");
		 set_nextSibling(leftSibling,(yyvsp[(4) - (5)].node));
		 Setattr((yyvsp[(1) - (5)].node),"_last",(yyvsp[(4) - (5)].node));
		 (yyval.node) = (yyvsp[(1) - (5)].node);
	       }
    break;

  case 415:

/* Line 1464 of yacc.c  */
#line 5900 "parser.y"
    {
		 (yyval.node) = (yyvsp[(1) - (3)].node);
	       }
    break;

  case 416:

/* Line 1464 of yacc.c  */
#line 5903 "parser.y"
    {
		 Setattr((yyvsp[(2) - (3)].node),"_last",(yyvsp[(2) - (3)].node));
		 (yyval.node) = (yyvsp[(2) - (3)].node);
	       }
    break;

  case 417:

/* Line 1464 of yacc.c  */
#line 5907 "parser.y"
    {
		 (yyval.node) = 0;
	       }
    break;

  case 418:

/* Line 1464 of yacc.c  */
#line 5912 "parser.y"
    {
		   SwigType *type = NewSwigType(T_INT);
		   (yyval.node) = new_node("enumitem");
		   Setattr((yyval.node),"name",(yyvsp[(1) - (1)].id));
		   Setattr((yyval.node),"type",type);
		   SetFlag((yyval.node),"feature:immutable");
		   Delete(type);
		 }
    break;

  case 419:

/* Line 1464 of yacc.c  */
#line 5920 "parser.y"
    {
		   SwigType *type = NewSwigType((yyvsp[(3) - (3)].dtype).type == T_BOOL ? T_BOOL : ((yyvsp[(3) - (3)].dtype).type == T_CHAR ? T_CHAR : T_INT));
		   (yyval.node) = new_node("enumitem");
		   Setattr((yyval.node),"name",(yyvsp[(1) - (3)].id));
		   Setattr((yyval.node),"type",type);
		   SetFlag((yyval.node),"feature:immutable");
		   Setattr((yyval.node),"enumvalue", (yyvsp[(3) - (3)].dtype).val);
		   Setattr((yyval.node),"value",(yyvsp[(1) - (3)].id));
		   Delete(type);
                 }
    break;

  case 420:

/* Line 1464 of yacc.c  */
#line 5932 "parser.y"
    {
                   (yyval.dtype) = (yyvsp[(1) - (1)].dtype);
		   if (((yyval.dtype).type != T_INT) && ((yyval.dtype).type != T_UINT) &&
		       ((yyval.dtype).type != T_LONG) && ((yyval.dtype).type != T_ULONG) &&
		       ((yyval.dtype).type != T_LONGLONG) && ((yyval.dtype).type != T_ULONGLONG) &&
		       ((yyval.dtype).type != T_SHORT) && ((yyval.dtype).type != T_USHORT) &&
		       ((yyval.dtype).type != T_SCHAR) && ((yyval.dtype).type != T_UCHAR) &&
		       ((yyval.dtype).type != T_CHAR) && ((yyval.dtype).type != T_BOOL)) {
		     Swig_error(cparse_file,cparse_line,"Type error. Expecting an integral type\n");
		   }
                }
    break;

  case 421:

/* Line 1464 of yacc.c  */
#line 5947 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 422:

/* Line 1464 of yacc.c  */
#line 5948 "parser.y"
    {
		 Node *n;
		 (yyval.dtype).val = (yyvsp[(1) - (1)].type);
		 (yyval.dtype).type = T_INT;
		 /* Check if value is in scope */
		 n = Swig_symbol_clookup((yyvsp[(1) - (1)].type),0);
		 if (n) {
                   /* A band-aid for enum values used in expressions. */
                   if (Strcmp(nodeType(n),"enumitem") == 0) {
                     String *q = Swig_symbol_qualified(n);
                     if (q) {
                       (yyval.dtype).val = NewStringf("%s::%s", q, Getattr(n,"name"));
                       Delete(q);
                     }
                   }
		 }
               }
    break;

  case 423:

/* Line 1464 of yacc.c  */
#line 5967 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 424:

/* Line 1464 of yacc.c  */
#line 5968 "parser.y"
    {
		    (yyval.dtype).val = (yyvsp[(1) - (1)].str);
                    (yyval.dtype).type = T_STRING;
               }
    break;

  case 425:

/* Line 1464 of yacc.c  */
#line 5972 "parser.y"
    {
		  SwigType_push((yyvsp[(3) - (5)].type),(yyvsp[(4) - (5)].decl).type);
		  (yyval.dtype).val = NewStringf("sizeof(%s)",SwigType_str((yyvsp[(3) - (5)].type),0));
		  (yyval.dtype).type = T_ULONG;
               }
    break;

  case 426:

/* Line 1464 of yacc.c  */
#line 5977 "parser.y"
    {
		  SwigType_push((yyvsp[(6) - (8)].type),(yyvsp[(7) - (8)].decl).type);
		  (yyval.dtype).val = NewStringf("sizeof...(%s)",SwigType_str((yyvsp[(6) - (8)].type),0));
		  (yyval.dtype).type = T_ULONG;
               }
    break;

  case 427:

/* Line 1464 of yacc.c  */
#line 5982 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 428:

/* Line 1464 of yacc.c  */
#line 5983 "parser.y"
    {
		    (yyval.dtype).val = (yyvsp[(1) - (1)].str);
		    (yyval.dtype).rawval = NewStringf("L\"%s\"", (yyval.dtype).val);
                    (yyval.dtype).type = T_WSTRING;
	       }
    break;

  case 429:

/* Line 1464 of yacc.c  */
#line 5988 "parser.y"
    {
		  (yyval.dtype).val = NewString((yyvsp[(1) - (1)].str));
		  if (Len((yyval.dtype).val)) {
		    (yyval.dtype).rawval = NewStringf("'%(escape)s'", (yyval.dtype).val);
		  } else {
		    (yyval.dtype).rawval = NewString("'\\0'");
		  }
		  (yyval.dtype).type = T_CHAR;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
	       }
    break;

  case 430:

/* Line 1464 of yacc.c  */
#line 6001 "parser.y"
    {
		  (yyval.dtype).val = NewString((yyvsp[(1) - (1)].str));
		  if (Len((yyval.dtype).val)) {
		    (yyval.dtype).rawval = NewStringf("L\'%s\'", (yyval.dtype).val);
		  } else {
		    (yyval.dtype).rawval = NewString("L'\\0'");
		  }
		  (yyval.dtype).type = T_WCHAR;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
	       }
    break;

  case 431:

/* Line 1464 of yacc.c  */
#line 6016 "parser.y"
    {
   	            (yyval.dtype).val = NewStringf("(%s)",(yyvsp[(2) - (3)].dtype).val);
		    (yyval.dtype).type = (yyvsp[(2) - (3)].dtype).type;
   	       }
    break;

  case 432:

/* Line 1464 of yacc.c  */
#line 6023 "parser.y"
    {
                 (yyval.dtype) = (yyvsp[(4) - (4)].dtype);
		 if ((yyvsp[(4) - (4)].dtype).type != T_STRING) {
		   switch ((yyvsp[(2) - (4)].dtype).type) {
		     case T_FLOAT:
		     case T_DOUBLE:
		     case T_LONGDOUBLE:
		     case T_FLTCPLX:
		     case T_DBLCPLX:
		       (yyval.dtype).val = NewStringf("(%s)%s", (yyvsp[(2) - (4)].dtype).val, (yyvsp[(4) - (4)].dtype).val); /* SwigType_str and decimal points don't mix! */
		       break;
		     default:
		       (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[(2) - (4)].dtype).val,0), (yyvsp[(4) - (4)].dtype).val);
		       break;
		   }
		 }
 	       }
    break;

  case 433:

/* Line 1464 of yacc.c  */
#line 6040 "parser.y"
    {
                 (yyval.dtype) = (yyvsp[(5) - (5)].dtype);
		 if ((yyvsp[(5) - (5)].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[(2) - (5)].dtype).val,(yyvsp[(3) - (5)].type));
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[(2) - (5)].dtype).val,0), (yyvsp[(5) - (5)].dtype).val);
		 }
 	       }
    break;

  case 434:

/* Line 1464 of yacc.c  */
#line 6047 "parser.y"
    {
                 (yyval.dtype) = (yyvsp[(5) - (5)].dtype);
		 if ((yyvsp[(5) - (5)].dtype).type != T_STRING) {
		   SwigType_add_reference((yyvsp[(2) - (5)].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[(2) - (5)].dtype).val,0), (yyvsp[(5) - (5)].dtype).val);
		 }
 	       }
    break;

  case 435:

/* Line 1464 of yacc.c  */
#line 6054 "parser.y"
    {
                 (yyval.dtype) = (yyvsp[(5) - (5)].dtype);
		 if ((yyvsp[(5) - (5)].dtype).type != T_STRING) {
		   SwigType_add_rvalue_reference((yyvsp[(2) - (5)].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[(2) - (5)].dtype).val,0), (yyvsp[(5) - (5)].dtype).val);
		 }
 	       }
    break;

  case 436:

/* Line 1464 of yacc.c  */
#line 6061 "parser.y"
    {
                 (yyval.dtype) = (yyvsp[(6) - (6)].dtype);
		 if ((yyvsp[(6) - (6)].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[(2) - (6)].dtype).val,(yyvsp[(3) - (6)].type));
		   SwigType_add_reference((yyvsp[(2) - (6)].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[(2) - (6)].dtype).val,0), (yyvsp[(6) - (6)].dtype).val);
		 }
 	       }
    break;

  case 437:

/* Line 1464 of yacc.c  */
#line 6069 "parser.y"
    {
                 (yyval.dtype) = (yyvsp[(6) - (6)].dtype);
		 if ((yyvsp[(6) - (6)].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[(2) - (6)].dtype).val,(yyvsp[(3) - (6)].type));
		   SwigType_add_rvalue_reference((yyvsp[(2) - (6)].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[(2) - (6)].dtype).val,0), (yyvsp[(6) - (6)].dtype).val);
		 }
 	       }
    break;

  case 438:

/* Line 1464 of yacc.c  */
#line 6077 "parser.y"
    {
		 (yyval.dtype) = (yyvsp[(2) - (2)].dtype);
                 (yyval.dtype).val = NewStringf("&%s",(yyvsp[(2) - (2)].dtype).val);
	       }
    break;

  case 439:

/* Line 1464 of yacc.c  */
#line 6081 "parser.y"
    {
		 (yyval.dtype) = (yyvsp[(2) - (2)].dtype);
                 (yyval.dtype).val = NewStringf("&&%s",(yyvsp[(2) - (2)].dtype).val);
	       }
    break;

  case 440:

/* Line 1464 of yacc.c  */
#line 6085 "parser.y"
    {
		 (yyval.dtype) = (yyvsp[(2) - (2)].dtype);
                 (yyval.dtype).val = NewStringf("*%s",(yyvsp[(2) - (2)].dtype).val);
	       }
    break;

  case 441:

/* Line 1464 of yacc.c  */
#line 6091 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 442:

/* Line 1464 of yacc.c  */
#line 6092 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 443:

/* Line 1464 of yacc.c  */
#line 6093 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 444:

/* Line 1464 of yacc.c  */
#line 6094 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 445:

/* Line 1464 of yacc.c  */
#line 6095 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 446:

/* Line 1464 of yacc.c  */
#line 6096 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 447:

/* Line 1464 of yacc.c  */
#line 6097 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 448:

/* Line 1464 of yacc.c  */
#line 6098 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 449:

/* Line 1464 of yacc.c  */
#line 6101 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s+%s", COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 450:

/* Line 1464 of yacc.c  */
#line 6105 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s-%s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 451:

/* Line 1464 of yacc.c  */
#line 6109 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s*%s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 452:

/* Line 1464 of yacc.c  */
#line 6113 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s/%s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 453:

/* Line 1464 of yacc.c  */
#line 6117 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s%%%s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 454:

/* Line 1464 of yacc.c  */
#line 6121 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s&%s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 455:

/* Line 1464 of yacc.c  */
#line 6125 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s|%s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 456:

/* Line 1464 of yacc.c  */
#line 6129 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s^%s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 457:

/* Line 1464 of yacc.c  */
#line 6133 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s << %s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = promote_type((yyvsp[(1) - (3)].dtype).type);
	       }
    break;

  case 458:

/* Line 1464 of yacc.c  */
#line 6137 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s >> %s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = promote_type((yyvsp[(1) - (3)].dtype).type);
	       }
    break;

  case 459:

/* Line 1464 of yacc.c  */
#line 6141 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s&&%s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 460:

/* Line 1464 of yacc.c  */
#line 6145 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s||%s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 461:

/* Line 1464 of yacc.c  */
#line 6149 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s==%s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 462:

/* Line 1464 of yacc.c  */
#line 6153 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s!=%s",COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)),COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 463:

/* Line 1464 of yacc.c  */
#line 6167 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s >= %s", COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)), COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 464:

/* Line 1464 of yacc.c  */
#line 6171 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s <= %s", COMPOUND_EXPR_VAL((yyvsp[(1) - (3)].dtype)), COMPOUND_EXPR_VAL((yyvsp[(3) - (3)].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 465:

/* Line 1464 of yacc.c  */
#line 6175 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s?%s:%s", COMPOUND_EXPR_VAL((yyvsp[(1) - (5)].dtype)), COMPOUND_EXPR_VAL((yyvsp[(3) - (5)].dtype)), COMPOUND_EXPR_VAL((yyvsp[(5) - (5)].dtype)));
		 /* This may not be exactly right, but is probably good enough
		  * for the purposes of parsing constant expressions. */
		 (yyval.dtype).type = promote((yyvsp[(3) - (5)].dtype).type, (yyvsp[(5) - (5)].dtype).type);
	       }
    break;

  case 466:

/* Line 1464 of yacc.c  */
#line 6181 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("-%s",(yyvsp[(2) - (2)].dtype).val);
		 (yyval.dtype).type = (yyvsp[(2) - (2)].dtype).type;
	       }
    break;

  case 467:

/* Line 1464 of yacc.c  */
#line 6185 "parser.y"
    {
                 (yyval.dtype).val = NewStringf("+%s",(yyvsp[(2) - (2)].dtype).val);
		 (yyval.dtype).type = (yyvsp[(2) - (2)].dtype).type;
	       }
    break;

  case 468:

/* Line 1464 of yacc.c  */
#line 6189 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("~%s",(yyvsp[(2) - (2)].dtype).val);
		 (yyval.dtype).type = (yyvsp[(2) - (2)].dtype).type;
	       }
    break;

  case 469:

/* Line 1464 of yacc.c  */
#line 6193 "parser.y"
    {
                 (yyval.dtype).val = NewStringf("!%s",COMPOUND_EXPR_VAL((yyvsp[(2) - (2)].dtype)));
		 (yyval.dtype).type = T_INT;
	       }
    break;

  case 470:

/* Line 1464 of yacc.c  */
#line 6197 "parser.y"
    {
		 String *qty;
                 skip_balanced('(',')');
		 qty = Swig_symbol_type_qualify((yyvsp[(1) - (2)].type),0);
		 if (SwigType_istemplate(qty)) {
		   String *nstr = SwigType_namestr(qty);
		   Delete(qty);
		   qty = nstr;
		 }
		 (yyval.dtype).val = NewStringf("%s%s",qty,scanner_ccode);
		 Clear(scanner_ccode);
		 (yyval.dtype).type = T_INT;
		 Delete(qty);
               }
    break;

  case 471:

/* Line 1464 of yacc.c  */
#line 6213 "parser.y"
    {
	        (yyval.str) = NewString("...");
	      }
    break;

  case 472:

/* Line 1464 of yacc.c  */
#line 6218 "parser.y"
    {
	        (yyval.str) = (yyvsp[(1) - (1)].str);
	      }
    break;

  case 473:

/* Line 1464 of yacc.c  */
#line 6221 "parser.y"
    {
	        (yyval.str) = 0;
	      }
    break;

  case 474:

/* Line 1464 of yacc.c  */
#line 6226 "parser.y"
    {
		 (yyval.bases) = (yyvsp[(1) - (1)].bases);
               }
    break;

  case 475:

/* Line 1464 of yacc.c  */
#line 6231 "parser.y"
    { inherit_list = 1; }
    break;

  case 476:

/* Line 1464 of yacc.c  */
#line 6231 "parser.y"
    { (yyval.bases) = (yyvsp[(3) - (3)].bases); inherit_list = 0; }
    break;

  case 477:

/* Line 1464 of yacc.c  */
#line 6232 "parser.y"
    { (yyval.bases) = 0; }
    break;

  case 478:

/* Line 1464 of yacc.c  */
#line 6235 "parser.y"
    {
		   Hash *list = NewHash();
		   Node *base = (yyvsp[(1) - (1)].node);
		   Node *name = Getattr(base,"name");
		   List *lpublic = NewList();
		   List *lprotected = NewList();
		   List *lprivate = NewList();
		   Setattr(list,"public",lpublic);
		   Setattr(list,"protected",lprotected);
		   Setattr(list,"private",lprivate);
		   Delete(lpublic);
		   Delete(lprotected);
		   Delete(lprivate);
		   Append(Getattr(list,Getattr(base,"access")),name);
	           (yyval.bases) = list;
               }
    break;

  case 479:

/* Line 1464 of yacc.c  */
#line 6252 "parser.y"
    {
		   Hash *list = (yyvsp[(1) - (3)].bases);
		   Node *base = (yyvsp[(3) - (3)].node);
		   Node *name = Getattr(base,"name");
		   Append(Getattr(list,Getattr(base,"access")),name);
                   (yyval.bases) = list;
               }
    break;

  case 480:

/* Line 1464 of yacc.c  */
#line 6261 "parser.y"
    {
		 (yyval.intvalue) = cparse_line;
	       }
    break;

  case 481:

/* Line 1464 of yacc.c  */
#line 6263 "parser.y"
    {
		 (yyval.node) = NewHash();
		 Setfile((yyval.node),cparse_file);
		 Setline((yyval.node),(yyvsp[(2) - (4)].intvalue));
		 Setattr((yyval.node),"name",(yyvsp[(3) - (4)].str));
		 Setfile((yyvsp[(3) - (4)].str),cparse_file);
		 Setline((yyvsp[(3) - (4)].str),(yyvsp[(2) - (4)].intvalue));
                 if (last_cpptype && (Strcmp(last_cpptype,"struct") != 0)) {
		   Setattr((yyval.node),"access","private");
		   Swig_warning(WARN_PARSE_NO_ACCESS, Getfile((yyval.node)), Getline((yyval.node)), "No access specifier given for base class '%s' (ignored).\n", SwigType_namestr((yyvsp[(3) - (4)].str)));
                 } else {
		   Setattr((yyval.node),"access","public");
		 }
		 if ((yyvsp[(4) - (4)].str))
		   SetFlag((yyval.node), "variadic");
               }
    break;

  case 482:

/* Line 1464 of yacc.c  */
#line 6279 "parser.y"
    {
		 (yyval.intvalue) = cparse_line;
	       }
    break;

  case 483:

/* Line 1464 of yacc.c  */
#line 6281 "parser.y"
    {
		 (yyval.node) = NewHash();
		 Setfile((yyval.node),cparse_file);
		 Setline((yyval.node),(yyvsp[(3) - (6)].intvalue));
		 Setattr((yyval.node),"name",(yyvsp[(5) - (6)].str));
		 Setfile((yyvsp[(5) - (6)].str),cparse_file);
		 Setline((yyvsp[(5) - (6)].str),(yyvsp[(3) - (6)].intvalue));
		 Setattr((yyval.node),"access",(yyvsp[(2) - (6)].id));
	         if (Strcmp((yyvsp[(2) - (6)].id),"public") != 0) {
		   Swig_warning(WARN_PARSE_PRIVATE_INHERIT, Getfile((yyval.node)), Getline((yyval.node)), "%s inheritance from base '%s' (ignored).\n", (yyvsp[(2) - (6)].id), SwigType_namestr((yyvsp[(5) - (6)].str)));
		 }
		 if ((yyvsp[(6) - (6)].str))
		   SetFlag((yyval.node), "variadic");
               }
    break;

  case 484:

/* Line 1464 of yacc.c  */
#line 6297 "parser.y"
    { (yyval.id) = (char*)"public"; }
    break;

  case 485:

/* Line 1464 of yacc.c  */
#line 6298 "parser.y"
    { (yyval.id) = (char*)"private"; }
    break;

  case 486:

/* Line 1464 of yacc.c  */
#line 6299 "parser.y"
    { (yyval.id) = (char*)"protected"; }
    break;

  case 487:

/* Line 1464 of yacc.c  */
#line 6303 "parser.y"
    { 
                   (yyval.id) = (char*)"class"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
    break;

  case 488:

/* Line 1464 of yacc.c  */
#line 6307 "parser.y"
    { 
                   (yyval.id) = (char *)"typename"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
    break;

  case 489:

/* Line 1464 of yacc.c  */
#line 6311 "parser.y"
    { 
                   (yyval.id) = (char *)"class..."; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
    break;

  case 490:

/* Line 1464 of yacc.c  */
#line 6315 "parser.y"
    { 
                   (yyval.id) = (char *)"typename..."; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
    break;

  case 491:

/* Line 1464 of yacc.c  */
#line 6321 "parser.y"
    {
                 (yyval.id) = (yyvsp[(1) - (1)].id);
               }
    break;

  case 492:

/* Line 1464 of yacc.c  */
#line 6324 "parser.y"
    { 
                   (yyval.id) = (char*)"struct"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
    break;

  case 493:

/* Line 1464 of yacc.c  */
#line 6328 "parser.y"
    {
                   (yyval.id) = (char*)"union"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
    break;

  case 496:

/* Line 1464 of yacc.c  */
#line 6338 "parser.y"
    {
                   (yyval.str) = 0;
	       }
    break;

  case 497:

/* Line 1464 of yacc.c  */
#line 6341 "parser.y"
    {
                   (yyval.str) = 0;
	       }
    break;

  case 498:

/* Line 1464 of yacc.c  */
#line 6344 "parser.y"
    {
                   (yyval.str) = 0;
	       }
    break;

  case 499:

/* Line 1464 of yacc.c  */
#line 6347 "parser.y"
    {
                   (yyval.str) = 0;
	       }
    break;

  case 500:

/* Line 1464 of yacc.c  */
#line 6352 "parser.y"
    {
                    (yyval.dtype).throws = (yyvsp[(3) - (4)].pl);
                    (yyval.dtype).throwf = NewString("1");
                    (yyval.dtype).nexcept = 0;
	       }
    break;

  case 501:

/* Line 1464 of yacc.c  */
#line 6357 "parser.y"
    {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = NewString("true");
	       }
    break;

  case 502:

/* Line 1464 of yacc.c  */
#line 6362 "parser.y"
    {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = 0;
	       }
    break;

  case 503:

/* Line 1464 of yacc.c  */
#line 6367 "parser.y"
    {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = NewString("true");
	       }
    break;

  case 504:

/* Line 1464 of yacc.c  */
#line 6372 "parser.y"
    {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = (yyvsp[(3) - (4)].dtype).val;
	       }
    break;

  case 505:

/* Line 1464 of yacc.c  */
#line 6379 "parser.y"
    {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = 0;
                    (yyval.dtype).qualifier = (yyvsp[(1) - (1)].str);
               }
    break;

  case 506:

/* Line 1464 of yacc.c  */
#line 6385 "parser.y"
    {
		    (yyval.dtype) = (yyvsp[(1) - (1)].dtype);
                    (yyval.dtype).qualifier = 0;
               }
    break;

  case 507:

/* Line 1464 of yacc.c  */
#line 6389 "parser.y"
    {
		    (yyval.dtype) = (yyvsp[(2) - (2)].dtype);
                    (yyval.dtype).qualifier = (yyvsp[(1) - (2)].str);
               }
    break;

  case 508:

/* Line 1464 of yacc.c  */
#line 6393 "parser.y"
    { 
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = 0;
                    (yyval.dtype).qualifier = 0; 
               }
    break;

  case 509:

/* Line 1464 of yacc.c  */
#line 6401 "parser.y"
    { 
                    Clear(scanner_ccode); 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = 0; 
		    (yyval.decl).throws = (yyvsp[(1) - (3)].dtype).throws;
		    (yyval.decl).throwf = (yyvsp[(1) - (3)].dtype).throwf;
		    (yyval.decl).nexcept = (yyvsp[(1) - (3)].dtype).nexcept;
               }
    break;

  case 510:

/* Line 1464 of yacc.c  */
#line 6409 "parser.y"
    { 
                    skip_balanced('{','}'); 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = 0; 
                    (yyval.decl).throws = (yyvsp[(1) - (3)].dtype).throws;
                    (yyval.decl).throwf = (yyvsp[(1) - (3)].dtype).throwf;
                    (yyval.decl).nexcept = (yyvsp[(1) - (3)].dtype).nexcept;
               }
    break;

  case 511:

/* Line 1464 of yacc.c  */
#line 6417 "parser.y"
    { 
                    Clear(scanner_ccode); 
                    (yyval.decl).parms = (yyvsp[(2) - (4)].pl); 
                    (yyval.decl).have_parms = 1; 
                    (yyval.decl).defarg = 0; 
		    (yyval.decl).throws = 0;
		    (yyval.decl).throwf = 0;
		    (yyval.decl).nexcept = 0;
               }
    break;

  case 512:

/* Line 1464 of yacc.c  */
#line 6426 "parser.y"
    {
                    skip_balanced('{','}'); 
                    (yyval.decl).parms = (yyvsp[(2) - (4)].pl); 
                    (yyval.decl).have_parms = 1; 
                    (yyval.decl).defarg = 0; 
                    (yyval.decl).throws = 0;
                    (yyval.decl).throwf = 0;
                    (yyval.decl).nexcept = 0;
               }
    break;

  case 513:

/* Line 1464 of yacc.c  */
#line 6435 "parser.y"
    { 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = (yyvsp[(2) - (3)].dtype).val; 
                    (yyval.decl).throws = 0;
                    (yyval.decl).throwf = 0;
                    (yyval.decl).nexcept = 0;
               }
    break;

  case 514:

/* Line 1464 of yacc.c  */
#line 6442 "parser.y"
    {
                    (yyval.decl).have_parms = 0;
                    (yyval.decl).defarg = (yyvsp[(3) - (4)].dtype).val;
                    (yyval.decl).throws = (yyvsp[(1) - (4)].dtype).throws;
                    (yyval.decl).throwf = (yyvsp[(1) - (4)].dtype).throwf;
                    (yyval.decl).nexcept = (yyvsp[(1) - (4)].dtype).nexcept;
               }
    break;

  case 521:

/* Line 1464 of yacc.c  */
#line 6461 "parser.y"
    {
		  skip_balanced('(',')');
		  Clear(scanner_ccode);
		}
    break;

  case 522:

/* Line 1464 of yacc.c  */
#line 6473 "parser.y"
    {
		  skip_balanced('{','}');
		  Clear(scanner_ccode);
		}
    break;

  case 523:

/* Line 1464 of yacc.c  */
#line 6479 "parser.y"
    {
                     String *s = NewStringEmpty();
                     SwigType_add_template(s,(yyvsp[(2) - (3)].p));
                     (yyval.id) = Char(s);
		     scanner_last_id(1);
                }
    break;

  case 524:

/* Line 1464 of yacc.c  */
#line 6488 "parser.y"
    { (yyval.id) = (yyvsp[(1) - (1)].id); }
    break;

  case 525:

/* Line 1464 of yacc.c  */
#line 6489 "parser.y"
    { (yyval.id) = Swig_copy_string("override"); }
    break;

  case 526:

/* Line 1464 of yacc.c  */
#line 6490 "parser.y"
    { (yyval.id) = Swig_copy_string("final"); }
    break;

  case 527:

/* Line 1464 of yacc.c  */
#line 6493 "parser.y"
    { (yyval.id) = (yyvsp[(1) - (1)].id); }
    break;

  case 528:

/* Line 1464 of yacc.c  */
#line 6494 "parser.y"
    { (yyval.id) = Char((yyvsp[(1) - (1)].dtype).val); }
    break;

  case 529:

/* Line 1464 of yacc.c  */
#line 6495 "parser.y"
    { (yyval.id) = Char((yyvsp[(1) - (1)].str)); }
    break;

  case 530:

/* Line 1464 of yacc.c  */
#line 6498 "parser.y"
    { (yyval.id) = (yyvsp[(1) - (1)].id); }
    break;

  case 531:

/* Line 1464 of yacc.c  */
#line 6499 "parser.y"
    { (yyval.id) = 0; }
    break;

  case 532:

/* Line 1464 of yacc.c  */
#line 6502 "parser.y"
    { 
                  (yyval.str) = 0;
		  if (!(yyval.str)) (yyval.str) = NewStringf("%s%s", (yyvsp[(1) - (2)].str),(yyvsp[(2) - (2)].str));
      	          Delete((yyvsp[(2) - (2)].str));
               }
    break;

  case 533:

/* Line 1464 of yacc.c  */
#line 6507 "parser.y"
    {
		 (yyval.str) = NewStringf("::%s%s",(yyvsp[(3) - (4)].str),(yyvsp[(4) - (4)].str));
                 Delete((yyvsp[(4) - (4)].str));
               }
    break;

  case 534:

/* Line 1464 of yacc.c  */
#line 6511 "parser.y"
    {
		 (yyval.str) = NewString((yyvsp[(1) - (1)].str));
   	       }
    break;

  case 535:

/* Line 1464 of yacc.c  */
#line 6514 "parser.y"
    {
		 (yyval.str) = NewStringf("::%s",(yyvsp[(3) - (3)].str));
               }
    break;

  case 536:

/* Line 1464 of yacc.c  */
#line 6517 "parser.y"
    {
                 (yyval.str) = NewStringf("%s", (yyvsp[(1) - (1)].str));
	       }
    break;

  case 537:

/* Line 1464 of yacc.c  */
#line 6520 "parser.y"
    {
                 (yyval.str) = NewStringf("%s%s", (yyvsp[(1) - (2)].str), (yyvsp[(2) - (2)].id));
	       }
    break;

  case 538:

/* Line 1464 of yacc.c  */
#line 6523 "parser.y"
    {
                 (yyval.str) = NewStringf("::%s",(yyvsp[(3) - (3)].str));
               }
    break;

  case 539:

/* Line 1464 of yacc.c  */
#line 6528 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s%s",(yyvsp[(2) - (3)].str),(yyvsp[(3) - (3)].str));
		   Delete((yyvsp[(3) - (3)].str));
               }
    break;

  case 540:

/* Line 1464 of yacc.c  */
#line 6532 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[(2) - (2)].str));
               }
    break;

  case 541:

/* Line 1464 of yacc.c  */
#line 6535 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[(2) - (2)].str));
               }
    break;

  case 542:

/* Line 1464 of yacc.c  */
#line 6542 "parser.y"
    {
		 (yyval.str) = NewStringf("::~%s",(yyvsp[(2) - (2)].str));
               }
    break;

  case 543:

/* Line 1464 of yacc.c  */
#line 6548 "parser.y"
    {
		(yyval.str) = NewStringf("%s", (yyvsp[(1) - (1)].id));
	      }
    break;

  case 544:

/* Line 1464 of yacc.c  */
#line 6551 "parser.y"
    {
		(yyval.str) = NewStringf("%s%s", (yyvsp[(1) - (2)].id), (yyvsp[(2) - (2)].id));
	      }
    break;

  case 545:

/* Line 1464 of yacc.c  */
#line 6556 "parser.y"
    {
		(yyval.str) = (yyvsp[(1) - (1)].str);
	      }
    break;

  case 546:

/* Line 1464 of yacc.c  */
#line 6559 "parser.y"
    {
		(yyval.str) = NewStringf("%s%s", (yyvsp[(2) - (3)].id), (yyvsp[(3) - (3)].id));
	      }
    break;

  case 547:

/* Line 1464 of yacc.c  */
#line 6565 "parser.y"
    {
                  (yyval.str) = 0;
		  if (!(yyval.str)) (yyval.str) = NewStringf("%s%s", (yyvsp[(1) - (2)].id),(yyvsp[(2) - (2)].str));
      	          Delete((yyvsp[(2) - (2)].str));
               }
    break;

  case 548:

/* Line 1464 of yacc.c  */
#line 6570 "parser.y"
    {
		 (yyval.str) = NewStringf("::%s%s",(yyvsp[(3) - (4)].id),(yyvsp[(4) - (4)].str));
                 Delete((yyvsp[(4) - (4)].str));
               }
    break;

  case 549:

/* Line 1464 of yacc.c  */
#line 6574 "parser.y"
    {
		 (yyval.str) = NewString((yyvsp[(1) - (1)].id));
   	       }
    break;

  case 550:

/* Line 1464 of yacc.c  */
#line 6577 "parser.y"
    {
		 (yyval.str) = NewStringf("::%s",(yyvsp[(3) - (3)].id));
               }
    break;

  case 551:

/* Line 1464 of yacc.c  */
#line 6580 "parser.y"
    {
                 (yyval.str) = NewString((yyvsp[(1) - (1)].str));
	       }
    break;

  case 552:

/* Line 1464 of yacc.c  */
#line 6583 "parser.y"
    {
                 (yyval.str) = NewStringf("::%s",(yyvsp[(3) - (3)].str));
               }
    break;

  case 553:

/* Line 1464 of yacc.c  */
#line 6588 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s%s",(yyvsp[(2) - (3)].id),(yyvsp[(3) - (3)].str));
		   Delete((yyvsp[(3) - (3)].str));
               }
    break;

  case 554:

/* Line 1464 of yacc.c  */
#line 6592 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[(2) - (2)].id));
               }
    break;

  case 555:

/* Line 1464 of yacc.c  */
#line 6595 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[(2) - (2)].str));
               }
    break;

  case 556:

/* Line 1464 of yacc.c  */
#line 6598 "parser.y"
    {
		 (yyval.str) = NewStringf("::~%s",(yyvsp[(2) - (2)].id));
               }
    break;

  case 557:

/* Line 1464 of yacc.c  */
#line 6604 "parser.y"
    { 
                   (yyval.str) = NewStringf("%s%s", (yyvsp[(1) - (2)].str), (yyvsp[(2) - (2)].id));
               }
    break;

  case 558:

/* Line 1464 of yacc.c  */
#line 6607 "parser.y"
    { (yyval.str) = NewString((yyvsp[(1) - (1)].id));}
    break;

  case 559:

/* Line 1464 of yacc.c  */
#line 6610 "parser.y"
    {
                   (yyval.str) = NewStringf("%s%s", (yyvsp[(1) - (2)].str), (yyvsp[(2) - (2)].id));
               }
    break;

  case 560:

/* Line 1464 of yacc.c  */
#line 6618 "parser.y"
    { (yyval.str) = NewString((yyvsp[(1) - (1)].id));}
    break;

  case 561:

/* Line 1464 of yacc.c  */
#line 6621 "parser.y"
    {
		 (yyval.str) = (yyvsp[(1) - (1)].str);
               }
    break;

  case 562:

/* Line 1464 of yacc.c  */
#line 6624 "parser.y"
    {
                  skip_balanced('{','}');
		  (yyval.str) = NewString(scanner_ccode);
               }
    break;

  case 563:

/* Line 1464 of yacc.c  */
#line 6628 "parser.y"
    {
		 (yyval.str) = (yyvsp[(1) - (1)].str);
              }
    break;

  case 564:

/* Line 1464 of yacc.c  */
#line 6633 "parser.y"
    {
                  Hash *n;
                  (yyval.node) = NewHash();
                  n = (yyvsp[(2) - (3)].node);
                  while(n) {
                     String *name, *value;
                     name = Getattr(n,"name");
                     value = Getattr(n,"value");
		     if (!value) value = (String *) "1";
                     Setattr((yyval.node),name, value);
		     n = nextSibling(n);
		  }
               }
    break;

  case 565:

/* Line 1464 of yacc.c  */
#line 6646 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 566:

/* Line 1464 of yacc.c  */
#line 6650 "parser.y"
    {
		 (yyval.node) = NewHash();
		 Setattr((yyval.node),"name",(yyvsp[(1) - (3)].id));
		 Setattr((yyval.node),"value",(yyvsp[(3) - (3)].str));
               }
    break;

  case 567:

/* Line 1464 of yacc.c  */
#line 6655 "parser.y"
    {
		 (yyval.node) = NewHash();
		 Setattr((yyval.node),"name",(yyvsp[(1) - (5)].id));
		 Setattr((yyval.node),"value",(yyvsp[(3) - (5)].str));
		 set_nextSibling((yyval.node),(yyvsp[(5) - (5)].node));
               }
    break;

  case 568:

/* Line 1464 of yacc.c  */
#line 6661 "parser.y"
    {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"name",(yyvsp[(1) - (1)].id));
	       }
    break;

  case 569:

/* Line 1464 of yacc.c  */
#line 6665 "parser.y"
    {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"name",(yyvsp[(1) - (3)].id));
                 set_nextSibling((yyval.node),(yyvsp[(3) - (3)].node));
               }
    break;

  case 570:

/* Line 1464 of yacc.c  */
#line 6670 "parser.y"
    {
                 (yyval.node) = (yyvsp[(3) - (3)].node);
		 Setattr((yyval.node),"name",(yyvsp[(1) - (3)].id));
               }
    break;

  case 571:

/* Line 1464 of yacc.c  */
#line 6674 "parser.y"
    {
                 (yyval.node) = (yyvsp[(3) - (5)].node);
		 Setattr((yyval.node),"name",(yyvsp[(1) - (5)].id));
		 set_nextSibling((yyval.node),(yyvsp[(5) - (5)].node));
               }
    break;

  case 572:

/* Line 1464 of yacc.c  */
#line 6681 "parser.y"
    {
		 (yyval.str) = (yyvsp[(1) - (1)].str);
               }
    break;

  case 573:

/* Line 1464 of yacc.c  */
#line 6684 "parser.y"
    {
                 (yyval.str) = Char((yyvsp[(1) - (1)].dtype).val);
               }
    break;



/* Line 1464 of yacc.c  */
#line 12776 "CParse/parser.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1684 of yacc.c  */
#line 6691 "parser.y"


SwigType *Swig_cparse_type(String *s) {
   String *ns;
   ns = NewStringf("%s;",s);
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSETYPE);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   return top;
}


Parm *Swig_cparse_parm(String *s) {
   String *ns;
   ns = NewStringf("%s;",s);
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSEPARM);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   Delete(ns);
   return top;
}


ParmList *Swig_cparse_parms(String *s, Node *file_line_node) {
   String *ns;
   char *cs = Char(s);
   if (cs && cs[0] != '(') {
     ns = NewStringf("(%s);",s);
   } else {
     ns = NewStringf("%s;",s);
   }
   Setfile(ns, Getfile(file_line_node));
   Setline(ns, Getline(file_line_node));
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSEPARMS);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   return top;
}


