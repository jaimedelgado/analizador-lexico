package alex;

import java.io.FileInputStream;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class AnalizadorLexicoTiny {
   private Map<ClaseLexica, String> palabrasReservadas;
   private Reader input;
   private StringBuffer lex;
   private int sigCar;
   private int filaInicio;
   private int columnaInicio;
   private int filaActual;
   private int columnaActual;
   private static String NL = System.getProperty("line.separator");
   
   private static enum Estado {
    INICIO, POR, NOT, DISTINTO, DIV, MOD, BARRA_OR, OR, COMA, SEPARADOR, PYCOMA, REFERENCIA, AND, CAP, VAR, CERO, PUNTODECIMAL, EOF, 
    NUMREAL, NUMNATURAL, PUNTO, CCIERRE, MENOR, MENORIGUAL, PAP, PCIERRE, MAYOR, MAYORIGUAL, APOSTROFE, MAS, ASIG, IGUAL, MENOS
   }

   private Estado estado;

   public AnalizadorLexicoTiny(Reader input) throws IOException {
    this.input = input;
    lex = new StringBuffer();
    sigCar = input.read();
    filaActual=1;
    columnaActual=1;
    this.palabrasReservadas = new HashMap<ClaseLexica, String>();
    this.inicializaTabla();
   }
   private void inicializaTabla(){
	   this.palabrasReservadas.put(ClaseLexica.TIPO, Constantes.TIPO);
	   this.palabrasReservadas.put(ClaseLexica.INT, Constantes.INT);
	   this.palabrasReservadas.put(ClaseLexica.REAL, Constantes.REAL);
	   this.palabrasReservadas.put(ClaseLexica.REC, Constantes.REC);
	   this.palabrasReservadas.put(ClaseLexica.ENDREC, Constantes.ENDREC);
	   this.palabrasReservadas.put(ClaseLexica.POINTER, Constantes.POINTER);
	   this.palabrasReservadas.put(ClaseLexica.OBJECT, Constantes.OBJECT);
	   this.palabrasReservadas.put(ClaseLexica.EXTENDS, Constantes.EXTENDS);
	   this.palabrasReservadas.put(ClaseLexica.ENDOBJECT, Constantes.ENDOBJECT);
	   this.palabrasReservadas.put(ClaseLexica.FUN, Constantes.FUN);
	   this.palabrasReservadas.put(ClaseLexica.METHOD, Constantes.METHOD);
	   this.palabrasReservadas.put(ClaseLexica.RETURNS, Constantes.RETURNS);
	   this.palabrasReservadas.put(ClaseLexica.RETURN, Constantes.RETURN);
	   this.palabrasReservadas.put(ClaseLexica.END, Constantes.END);
	   this.palabrasReservadas.put(ClaseLexica.THIS, Constantes.THIS);
	   this.palabrasReservadas.put(ClaseLexica.SUPER, Constantes.SUPER);
	   this.palabrasReservadas.put(ClaseLexica.NULL, Constantes.NULL);
	   this.palabrasReservadas.put(ClaseLexica.IN, Constantes.IN);
	   this.palabrasReservadas.put(ClaseLexica.OUT, Constantes.OUT);
	   this.palabrasReservadas.put(ClaseLexica.ALLOC, Constantes.ALLOC);
	   this.palabrasReservadas.put(ClaseLexica.FREE, Constantes.FREE);
	   this.palabrasReservadas.put(ClaseLexica.IF, Constantes.IF);
	   this.palabrasReservadas.put(ClaseLexica.ELSE, Constantes.ELSE);
	   this.palabrasReservadas.put(ClaseLexica.THEN, Constantes.THEN);
	   this.palabrasReservadas.put(ClaseLexica.ELSIF, Constantes.ELSIF);
	   this.palabrasReservadas.put(ClaseLexica.WHILE, Constantes.WHILE);
	   this.palabrasReservadas.put(ClaseLexica.DO, Constantes.DO);
	   this.palabrasReservadas.put(ClaseLexica.ENDIF, Constantes.ENDIF);
	   this.palabrasReservadas.put(ClaseLexica.ENDWHILE, Constantes.ENDWHILE);
	   this.palabrasReservadas.put(ClaseLexica.CERO, "cero");
	   
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
              else if (hayVar()) transita(Estado.VAR);
              else if (hayCero()) transita(Estado.CERO);
              else if (hayNumNaturalDigitoPos()) transita(Estado.NUMNATURAL);
              else if (hayPunto()) transitaIgnorando(Estado.PUNTO);
              else if (hayCCierre()) transitaIgnorando(Estado.CCIERRE);
              else if (hayMenor()) transita(Estado.MENOR);
              else if (hayPAp()) transita(Estado.PAP);
              else if (hayPCierre()) transita(Estado.PCIERRE);
              else if (hayMayor()) transita(Estado.MAYOR);
              else if (hayApostrofe()) transita(Estado.APOSTROFE);
              else if (hayMas()) transita(Estado.MAS);
              else if (hayAsig()) transita(Estado.ASIG);
              else if (hayMenos()) transita(Estado.MENOS);
              else if (hayPor()) transita(Estado.POR);
              else if (hayNot()) transita(Estado.NOT);
              else if (hayEOF()) transita(Estado.EOF);
              else error();
              break;
           case BARRA_OR: 
              if (hayOr()) transita(Estado.OR);    
              else if (hayEOF()) transita(Estado.EOF);
              else error();
              break;
           case NOT:
               if (hayDistinto()) transita(Estado.DISTINTO);
               else if (hayEOF()) transita(Estado.EOF);
               else return unidadNot();
               break;
           case ASIG:
               if (hayIgual()) transita(Estado.IGUAL);
               else if (hayEOF()) transita(Estado.EOF);
               else return unidadAsig();
               break;
           case MAYOR:
               if (hayMayorIgual()) transita(Estado.MAYORIGUAL);
               else if (hayEOF()) transita(Estado.EOF);
               else return unidadMayor();
               break;
           case MENOR: 
               if (hayMenorIgual()) transita(Estado.MENORIGUAL);
               else if (hayEOF()) transita(Estado.EOF);
               else return unidadMenor();
               break;
           case CERO: 
        	   if(hayPuntoDecimal()) transita(Estado.PUNTODECIMAL);
        	   else if (hayEOF()) transita(Estado.EOF);
        	   else return unidadCero();
        	   break;
           case NUMNATURAL: 
        	   if(hayNumNaturalDigito()) transita(Estado.NUMNATURAL);
        	   else if(hayPuntoDecimal()) transita(Estado.PUNTODECIMAL); 
        	   else if (hayEOF()) transita(Estado.EOF);
        	   else return unidadNumNatural();
        	   break;
           case PUNTODECIMAL: 
        	   if(hayNumNaturalDigito()) transita(Estado.NUMREAL);
        	   else if (hayEOF()) transita(Estado.EOF);
        	   else error();
        	   break;
           case NUMREAL: 
        	   if (hayNumNaturalDigitoPos()) transita(Estado.NUMREAL);
        	   else if (hayCero()) transita(Estado.PUNTODECIMAL);
        	   else if (hayEOF()) transita(Estado.EOF);
        	   else return unidadNumReal();
        	   break;
           case VAR: 
        	   if(hayVarConDigito()) transita(Estado.VAR);
        	   else if (hayEOF()) transita(Estado.EOF);
        	   else return unidadVar();
        	   break;
           case REFERENCIA: 
        	   if (hayAnd()) transita(Estado.AND);
        	   else if (hayEOF()) transita(Estado.EOF);
        	   else return unidadReferencia();
        	   break;
           case EOF: return unidadEOF();
         }
     }    
   }
   private UnidadLexica unidadReferencia() {
	   return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.REFERENCIA);     
   }
	private UnidadLexica unidadVar() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.NOMBRECAMPO);     
	}
	private UnidadLexica unidadNumReal() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.NUMREAL);     
	}
	private UnidadLexica unidadNumNatural() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.NUMNATURAL);     
	}
	private UnidadLexica unidadCero() {
		return new UnidadLexicaUnivaluada(filaInicio,columnaInicio,ClaseLexica.CERO);     
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
   private boolean hayDistinto() {return sigCar == '=';}
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
   private boolean hayVar() {return (sigCar >= 'a' && sigCar <='z') || (sigCar>='A' && sigCar<='Z') || sigCar == '_';}
   private boolean hayVarConDigito() { return hayVar() || (sigCar >= '0' && sigCar <= '9'); }
   private boolean hayIgnorable() {return sigCar == ' ' || sigCar == '\t' || sigCar=='\n';}
   private boolean hayCero() {return sigCar == '0';}
   private boolean hayPuntoDecimal() {return sigCar == '.';}
   private boolean hayNumReal() {return sigCar >= '0' && sigCar <= '9';}
   private boolean hayNumNaturalDigito() {return sigCar >= '0' && sigCar <= '9';}
   private boolean hayNumNaturalDigitoPos() {return sigCar > '0' && sigCar <= '9';}
   private boolean hayPunto() {return sigCar == '.';}
   private boolean hayCCierre() {return sigCar == ']';}
   private boolean hayMenor() {return sigCar == '<';}
   private boolean hayMenorIgual() {return sigCar == '=';}
   private boolean hayPAp() {return sigCar == '[';}
   private boolean hayPCierre() {return sigCar == ')';}
   private boolean hayMayor() {return sigCar == '>';}
   private boolean hayMayorIgual() {return sigCar == '=';}
   private boolean hayApostrofe() {return sigCar == '^';}
   private boolean hayMas() {return sigCar == '+';}
   private boolean hayAsig() {return sigCar == '=';}
   private boolean hayIgual() {return sigCar == '=';}
   private boolean hayMenos() {return sigCar == '-';}
   private boolean hayEOF() {return sigCar == -1;}
   
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
	         default:    
	            return null;//new UnidadLexicaMultivaluada(filaInicio,columnaInicio,null, null);//ClaseLexica.IDEN,lex.toString());     
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