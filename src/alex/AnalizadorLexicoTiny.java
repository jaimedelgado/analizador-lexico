package alex;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

public class AnalizadorLexicoTiny {
   private Reader input;
   private StringBuffer lex;
   private int sigCar;
   private int filaInicio;
   private int columnaInicio;
   private int filaActual;
   private int columnaActual;
   private static String NL = System.getProperty("line.separator");
   
   private static enum Estado {
    INICIO, SEPARADOR, POR, NOT, DISTINTO, DIV, MOD, BARRA_OR, OR, COMA, PYCOMA, REFERENCIA, AND, CAP, IDENTIFICADOR, CERO, PUNTODECIMAL, EOF, 
    NUMREAL, NUMENTERO, EXP, MENOSEXP, NUMREALEXP, PUNTO, CCIERRE, MENOR, MENORIGUAL, PAP, ICASTING, NCASTING, TCASTING,
    RCASTING, ECASTING, ACASTING, LCASTING,CASTINT, CASTREAL, PCIERRE, MAYOR, MAYORIGUAL, APOSTROFE, MAS, ASIG, IGUAL, MENOS,
    COMENTARIO, CEROEXP, CERODERECHA, 
   }

   private Estado estado;

   public AnalizadorLexicoTiny(Reader input) throws IOException {
    this.input = input;
    lex = new StringBuffer();
    sigCar = input.read();
    filaActual=1;
    columnaActual=1;
   }
   public UnidadLexica sigToken() throws IOException {
     estado = Estado.INICIO;
     filaInicio = filaActual;
     columnaInicio = columnaActual;
     lex.delete(0,lex.length());
     while(true) {
        switch(estado) {
           case INICIO: 
              if(hayDiv())  transita(Estado.DIV);
              else if (hayMod()) transita(Estado.MOD);
              else if (hayBarraOr()) transita(Estado.BARRA_OR);
              else if (hayComa()) transita(Estado.COMA);
              else if (haySeparador()) transita(Estado.SEPARADOR);
              else if (hayPycoma()) transita(Estado.PYCOMA);
              else if (hayReferencia()) transita(Estado.REFERENCIA);
              else if (hayCap()) transita(Estado.CAP);
              else if (hayIdentificador()) transita(Estado.IDENTIFICADOR);
              else if (hayCero()) transita(Estado.CERO);
              else if (hayDigitoPos()) transita(Estado.NUMENTERO);
              else if (hayPunto()) transitaIgnorando(Estado.PUNTO);
              else if (hayCCierre()) transitaIgnorando(Estado.CCIERRE);
              else if (hayMenor()) transita(Estado.MENOR);
              else if (hayPAp()) transita(Estado.PAP);
              else if (hayPCierre()) transita(Estado.PCIERRE);
              else if (hayMayor()) transita(Estado.MAYOR);
              else if (hayApostrofe()) transita(Estado.APOSTROFE);
              else if (hayMas()) transita(Estado.MAS);
              else if (hayIgual()) transita(Estado.ASIG);
              else if (hayMenos()) transita(Estado.MENOS);
              else if (hayPor()) transita(Estado.POR);
              else if (hayNot()) transita(Estado.NOT);
              else if (hayEOF()) transita(Estado.EOF);
              else if (hayComentario()) transita(Estado.COMENTARIO);
              else if(hayIgnorable()) transitaIgnorando(Estado.INICIO);
              else error();
              break;
           case BARRA_OR: 
              if (hayOr()) transita(Estado.OR);
              else error();
              break;
           case OR:
        	   return unidadOr();
           case COMA:
        	   return unidadComa();
           case SEPARADOR: 
        	   return unidadSep();
           case PYCOMA:
        	   return unidadPycoma();
           case REFERENCIA: 
        	   if (hayAnd()) transita(Estado.AND);
        	   else return unidadReferencia();
        	   break;
           case AND:
        	   return unidadAnd();
           case CAP:
        	   return unidadCap();
           case IDENTIFICADOR: 
        	   if(hayIdentificadorConDigito()) transita(Estado.IDENTIFICADOR);
        	   else return unidadId();
        	   break;
           case CERO: 
        	   if(hayPuntoDecimal()) transita(Estado.PUNTODECIMAL);
        	   else return unidadNumEntero();
        	   break;
           case PUNTODECIMAL: 
        	   if(hayDigito()) transita(Estado.NUMREAL);
        	   else error();
        	   break;
           case NUMREAL: 
        	   if (hayDigitoPos()) transita(Estado.NUMREAL);
        	   else if (hayE()) transita(Estado.EXP);
        	   else if (hayCero()) transita(Estado.CERODERECHA);
        	   else return unidadNumReal();
        	   break;
           case NUMENTERO: 
        	   if(hayDigito()) transita(Estado.NUMENTERO);
        	   else if(hayPunto()) transita(Estado.PUNTODECIMAL); 
        	   else if(hayE()) transita(Estado.EXP);
        	   else return unidadNumEntero();
        	   break;
           case EXP:
        	   if(hayDigitoPos()) transita(Estado.NUMREALEXP);
        	   else if(hayMenos()) transita(Estado.MENOSEXP);
        	   else if(hayCero()) transita(Estado.CEROEXP);
        	   else error();
        	   break;
           case MENOSEXP:
        	   if(hayDigitoPos()) transita(Estado.NUMREALEXP);
        	   else if(hayCero()) transita(Estado.CEROEXP);
        	   else error();
        	   break;
           case NUMREALEXP:
        	   if(hayDigito()) transita(Estado.NUMREALEXP);
        	   else return unidadNumRealExp();
        	   break;
           case PUNTO:
        	   return unidadPunto();
           case CCIERRE:
        	   return unidadCCierre();
           case MENOR: 
               if (hayIgual()) transita(Estado.MENORIGUAL);
               else return unidadMenor();
               break;
           case MENORIGUAL:
        	   return unidadMenorIgual();
           case PAP:
        	   if(hayI()) transita(Estado.ICASTING);
        	   else if (hayR()) transita(Estado.RCASTING);
        	   else return unidadPap();
        	   break;
           case ICASTING:
        	   if(hayN()) transita(Estado.NCASTING);
        	   else error();
        	   break;
           case NCASTING:
        	   if(hayT()) transita(Estado.TCASTING);
        	   else error();
        	   break;
           case TCASTING:
        	   if(hayPCierre()) transita(Estado.CASTINT);
        	   else error();
        	   break;
           case CASTINT:
        	   return unidadCastint();
           case RCASTING:
        	   if(hayE()) transita(Estado.ECASTING);
        	   else error();
        	   break;
           case ECASTING:
        	   if(hayA()) transita(Estado.ACASTING);
        	   else error();
        	   break;
           case ACASTING:
        	   if(hayL()) transita(Estado.LCASTING);
        	   else error();
        	   break;
           case LCASTING:
        	   if(hayPCierre())	transita(Estado.CASTREAL);
        	   else error();
        	   break;
           case CASTREAL:
        	   return unidadCastreal();
           case PCIERRE:
        	   return unidadPCierre();
           case MAYOR:
               if (hayIgual()) transita(Estado.MAYORIGUAL);
               else return unidadMayor();
               break;
           case MAYORIGUAL:
        	   return unidadMayorIgual();
           case APOSTROFE:
        	   return unidadApostrofe();
           case MAS:
        	   return unidadMas();
           case ASIG:
               if (hayIgual()) transita(Estado.IGUAL);
               else return unidadAsig();
               break;
           case IGUAL:
        	   return unidadIgual();
           case MENOS:
        	   return unidadMenos();
           case POR:
        	   return unidadPor();
           case NOT:
               if (hayIgual()) transita(Estado.DISTINTO);
               else return unidadNot();
               break;
           case DIV:
        	   return unidadDiv();
           case MOD:
        	   return unidadMod();
           case EOF: return unidadEOF();
           case CEROEXP:
        	   return unidadCeroExp();
           case COMENTARIO:
        	   if (hayNL()) transitaIgnorando(Estado.INICIO);
               else if (hayEOF()) transita(Estado.EOF);
               else transitaIgnorando(Estado.COMENTARIO);
               break;
           case CERODERECHA:
        	   if(hayCero()) transita(Estado.CERODERECHA);
        	   else if(hayDigitoPos()) transita(Estado.NUMREAL);
        	   else error();
        	   break;
           case DISTINTO:
        	   return unidadDistinto();
		default:
			break;
         }
     }    
   }
   
	private UnidadLexica unidadCeroExp() {
   		return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.NUMREAL, lex.toString());
	}
	private UnidadLexica unidadDistinto() {
   		return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.DISTINTO, lex.toString());
   	}
   	private UnidadLexica unidadNumRealExp() {
	   return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.NUMREAL, lex.toString());
	}
	private UnidadLexica unidadMod() {
	   return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.MOD);    
   }
	private UnidadLexica unidadDiv() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.DIV);    
	}
	private UnidadLexica unidadPor() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.POR);    
	}
	private UnidadLexica unidadMenos() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.MENOS);    
	}
	private UnidadLexica unidadIgual() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.IGUAL);    
	}
	private UnidadLexica unidadMas() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.MAS);    
	}
	private UnidadLexica unidadApostrofe() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.APOSTROFE);    
	}
	private UnidadLexica unidadMayorIgual() {
		return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.MAYORIGUAL, lex.toString());    
	}
	private UnidadLexica unidadPCierre() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.PCIERRE);    
	}
	private UnidadLexica unidadCastreal() {
		return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.CASTREAL, lex.toString());    
	}
	private UnidadLexica unidadCastint() {
		return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.CASTINT, lex.toString());    
	}
	private UnidadLexica unidadPap() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.PAP);    
	}
	private UnidadLexica unidadMenorIgual() {
		return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.MENORIGUAL, lex.toString());    
	}
	private UnidadLexica unidadCCierre() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.CCIERRE);    
	}
	private UnidadLexica unidadPunto() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.PUNTO);    
	}
	private UnidadLexica unidadCap() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.CAP);    
	}
	private UnidadLexica unidadAnd() {
		return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.AND, lex.toString());    
	}
	private UnidadLexica unidadPycoma() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.PYCOMA);    
	}
	private UnidadLexica unidadSep() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.SEPARADOR);    
	}
	private UnidadLexica unidadComa() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.COMA);    
	}
	private UnidadLexica unidadOr() {
		return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.OR, lex.toString());    
	}
	private UnidadLexica unidadReferencia() {
	   return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.REFERENCIA);     
   }
	private UnidadLexica unidadNumReal() {
		return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.NUMREAL, lex.toString());     
	}
	private UnidadLexica unidadNumEntero() {
		return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.NUMENTERO, lex.toString());     
	}
	private UnidadLexica unidadMenor() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.MENOR);     
	}
	private UnidadLexica unidadMayor() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.MAYOR);     
	}
	private UnidadLexica unidadAsig() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.ASIG);     
	}
	private UnidadLexica unidadNot() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.NOT);     
	}
   private void transita(Estado sig) throws IOException {
     lex.append((char)sigCar);
     sigCar();         
     estado = sig;
   }
   private void transitaIgnorando(Estado sig) throws IOException {
     sigCar();         
     filaInicio = filaActual;
     columnaInicio = columnaActual;
     estado = sig;
   }
   private void sigCar() throws IOException {
     sigCar = input.read();
     if (sigCar == NL.charAt(0)) saltaFinDeLinea();
     if (sigCar == '\n') {
        filaActual++;
        columnaActual=0;
     }
     else {
       columnaActual++;  
     }
   }
   private void saltaFinDeLinea() throws IOException {
      for (int i=1; i < NL.length(); i++) {
          sigCar = input.read();
          if (sigCar != NL.charAt(i)) error();
      }
      sigCar = '\n';
   }
   
   private boolean hayPor() {return sigCar == '*';}
   private boolean hayNot() {return sigCar == '!';}
   private boolean hayDiv() {return sigCar == '/';}
   private boolean hayMod() {return sigCar == '%';}
   private boolean hayBarraOr() {return sigCar == '|';}
   private boolean hayOr() {return sigCar == '|';}
   private boolean hayComa() {return sigCar == ',';}
   private boolean haySeparador() {return sigCar == '#';}
   private boolean hayPycoma() {return sigCar == ';';}
   private boolean hayReferencia() {return sigCar == '&';}
   private boolean hayAnd() {return sigCar == '&';}
   private boolean hayCap() {return sigCar == '[';}
   private boolean hayIdentificador() {return (sigCar >= 'a' && sigCar <='z') || (sigCar>='A' && sigCar<='Z') || sigCar == '_';}
   private boolean hayIdentificadorConDigito() { return hayIdentificador() || (sigCar >= '0' && sigCar <= '9'); }
   private boolean hayIgnorable() {return sigCar == ' ' || sigCar == '\t' || sigCar=='\n';}
   private boolean hayCero() {return sigCar == '0';}
   private boolean hayPuntoDecimal() {return sigCar == '.';}
   private boolean hayDigito() {return sigCar >= '0' && sigCar <= '9';}
   private boolean hayDigitoPos() {return sigCar > '0' && sigCar <= '9';}
   private boolean hayPunto() {return sigCar == '.';}
   private boolean hayCCierre() {return sigCar == ']';}
   private boolean hayMenor() {return sigCar == '<';}
   private boolean hayPAp() {return sigCar == '(';}
   private boolean hayPCierre() {return sigCar == ')';}
   private boolean hayMayor() {return sigCar == '>';}
   private boolean hayApostrofe() {return sigCar == '^';}
   private boolean hayMas() {return sigCar == '+';}
   private boolean hayIgual() {return sigCar == '=';}
   private boolean hayMenos() {return sigCar == '-';}
   private boolean hayEOF() {return sigCar == -1;}
   private boolean hayE(){ return sigCar == 'e' | sigCar == 'E'; }
   private boolean hayI(){ return sigCar == 'i' | sigCar == 'I'; }
   private boolean hayN(){ return sigCar == 'n' | sigCar == 'N'; }
   private boolean hayT(){ return sigCar == 't' | sigCar == 'T'; }
   private boolean hayR(){ return sigCar == 'r' | sigCar == 'R'; }
   private boolean hayA(){ return sigCar == 'a' | sigCar == 'A'; }
   private boolean hayL(){ return sigCar == 'l' | sigCar == 'L'; }
   private boolean hayNL() {return sigCar == '\r' || sigCar == '\b' || sigCar == '\n';}
   private boolean hayComentario() { return sigCar == '@'; }
   
   private UnidadLexica unidadId() {
	     switch(lex.toString()) {
	         case Constantes.TIPO:  
	            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.TIPO);
	         case Constantes.INT:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.INT);
	         case Constantes.REAL:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.REAL);
	         case Constantes.REC:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.REC);
	         case Constantes.ENDREC:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.ENDREC);
	         case Constantes.POINTER:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.POINTER);
	         case Constantes.OBJECT:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.OBJECT);
	         case Constantes.EXTENDS:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.EXTENDS);
	         case Constantes.ENDOBJECT:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.ENDOBJECT);
	         case Constantes.FUN:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.FUN);
	         case Constantes.METHOD:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.METHOD);
	         case Constantes.RETURN:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.RETURN);
	         case Constantes.RETURNS:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.RETURNS);
	         case Constantes.NULL:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.NULL);
	         case Constantes.IN:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.IN);
	         case Constantes.OUT:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.OUT);
	         case Constantes.ALLOC:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.ALLOC);
	         case Constantes.FREE:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.FREE);
	         case Constantes.IF:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.IF);
	         case Constantes.ELSE:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.ELSE);
	         case Constantes.THEN:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.THEN);
	         case Constantes.ELSIF:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.ELSIF);
	         case Constantes.ENDIF:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.ENDIF);
	         case Constantes.WHILE:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.WHILE);
	         case Constantes.DO:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.DO);
	         case Constantes.ENDWHILE:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.ENDWHILE);
	         case Constantes.END:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.END);
	         case Constantes.THIS:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.THIS);
	         case Constantes.SUPER:  
		            return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.SUPER);
	         default:    
	            return new UnidadLexicaMultivaluada(filaInicio,columnaInicio,ClaseLexica.IDENTIFICADOR,lex.toString());     
	      }
   }  
 
   private UnidadLexica unidadEOF() {
     return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.EOF);     
   }    
   private void error() {
     System.err.println("("+filaActual+','+columnaActual+"):Caracter inexperado");  
     System.exit(1);
   }

   public static void main(String arg[]) throws IOException {
     Reader input = new InputStreamReader(new FileInputStream("input.txt"));
     AnalizadorLexicoTiny al = new AnalizadorLexicoTiny(input);
     UnidadLexica unidad;
     do {
       unidad = al.sigToken();
       System.out.println(unidad);
     }
     while (unidad.clase() != ClaseLexica.EOF);
    } 
}