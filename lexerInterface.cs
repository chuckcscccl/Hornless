namespace Fussless {
using System;

public interface absLexer  // not used by Fussless
{
   LexToken next(); // returns null at eof
   int linenum();
   // translator to particular grammar   
   LexToken translate_token(LexToken t);
}

public interface AbstractLexer<ET>  // in use by Fussless
{
   RawToken next_lt();
   void set_shared(ET shared);
}

public interface RawTokenizer
{
   RawToken next_rt();
}

public class LexToken
{
   public string token_type;
   public object token_value;
   public int line;      // lines start at 0
   public int column;    // columns start at 0
   public int position;  // absolute beginning character position (yychar)
   public LexToken(string t, object v) {token_type=t; token_value=v;}
   public LexToken(string t, object v, int l, int c)
   {token_type=t; token_value=v; line=l; column=c;}
   public LexToken(string t, object v, int l, int c, int p)
   {token_type=t; token_value=v; line=l; column=c; position=p;}      
   public override string ToString() {return token_type+"("+token_value+")";}
   public string complete_info()
   {
     return ("LexToken: type "+token_type+", value ("+token_value+"), type "+token_value.GetType()+", line "+line+", column "+column+", position "+position);
   }
}

public class RawToken // deprecates LexToken - currently in use
{
  public readonly string token_name;
  public readonly string token_text;
  public readonly int line;      // lines start at 1
  public readonly int column;    // columns start at 1
  public readonly int position;  // absolute beginning position (yychar)
  public RawToken(string t, string v) {token_name=t; token_text=v;}
  public RawToken(string t, string v, int l, int c)
   {token_name=t; token_text=v; line=l; column=c;}
  public RawToken(string t, string v, int l, int c, int p) 
   {token_name=t; token_text=v; line=l; column=c; position=p;}      
   public override string ToString() {return token_name+"("+token_text+")";}
   public string complete_info()
   {
     return ("RawToken: type "+token_name+", value ("+token_text+"), line "+line+", column "+column+", position "+position);
   }
}// RawToken


public class Utility { // moved from specific .lex file
  public static void assert(bool expr)
    {

      /*if (!expr)
      throw new ApplicationException("Error: Assertion failed.");
      */
      if (!expr) Console.WriteLine("Assertion Failed");
    }
  
  private static String[] errorMsg = new String[]
    {
    "Error: Unmatched end-of-comment punctuation.",
    "Error: Unmatched start-of-comment punctuation.",
    "Error: Unclosed string.",
    "Error: Illegal character."
    };
  
  public const int E_ENDCOMMENT = 0; 
  public const int E_STARTCOMMENT = 1; 
  public const int E_UNCLOSEDSTR = 2; 
  public const int E_UNMATCHED = 3; 

  public static void error
    (
    int code
    )
    {
      Console.WriteLine(errorMsg[code]);
    }
} //Utility class




} // Fussless


/*
public class TerminalToken  // defined in F#
{
  public string sym;  // must match terminal symbol name in grammar
  public object svalue;   // semantic value - cheap polymorphism
  public int line;
  public int column;
  public TerminalToken(string s, AT v, int l, int c)
  {sym=s; svalue=v; line=l; column=c;}
}
*/
