with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

procedure Calculator is
   -- For variables
   package Variable_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Integer,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   
   use Variable_Maps;
   
   Variables : Variable_Maps.Map;
   
   -- Errors
   type Error_Type is (No_Error, Division_By_Zero, Undefined_Variable, Syntax_Error, Parentheses_Mismatch);
   
   -- Helper functions
   function Is_Digit(C : in Character) return Boolean is
   begin
      return C in '0'..'9';
   end Is_Digit;
   
   function Is_Alpha(C : in Character) return Boolean is
   begin
      return C in 'a'..'z' or C in 'A'..'Z';
   end Is_Alpha;
   
   function Is_Valid_Variable_Char(C : in Character) return Boolean is
   begin
      return Is_Alpha(C) or Is_Digit(C) or C = '_';
   end Is_Valid_Variable_Char;
   
   procedure Skip_Spaces(Expr : in String; Pos : in out Integer) is
   begin
      while Pos <= Expr'Last and then Expr(Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;
   end Skip_Spaces;
   
   function Is_Valid_Input(Str : String) return Boolean is
   begin
      for C of Str loop
         if not Is_Digit(C) and then
            not Is_Alpha(C) and then
            C /= ' ' and then 
            C /= '+' and then
            C /= '-' and then
            C /= '*' and then
            C /= '/' and then
            C /= '=' and then
            C /= '(' and then
            C /= ')' and then
            C /= '_' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid_Input;

   -- For seperation of numbers and variables
   function Parse_Number_Or_Variable(Expr : in String; Pos : in out Integer; Error : out Error_Type) return Integer;
   function Parse_Factor(Expr : in String; Pos : in out Integer; Error : out Error_Type) return Integer;
   function Parse_Term(Expr : in String; Pos : in out Integer; Error : out Error_Type) return Integer;
   function Parse_Expression(Expr : in String; Pos : in out Integer; Error : out Error_Type) return Integer;
   function Parse_Assignment(Expr : in String; Error : out Error_Type) return Boolean;
   function Evaluate(Expr : in String; Error : out Error_Type) return Integer;
   
   function Parse_Number_Or_Variable(Expr : in String; Pos : in out Integer; Error : out Error_Type) return Integer is
      Start : Integer := Pos;
      Is_Number : Boolean := True;
      Result : Integer;
   begin
      Error := No_Error;
      
      -- First char control
      if Pos > Expr'Last or else 
         (not Is_Digit(Expr(Pos)) and then not Is_Alpha(Expr(Pos))) then
         Error := Syntax_Error;
         return 0;
      end if;
      
      while Pos <= Expr'Last and then 
            (Is_Digit(Expr(Pos)) or else Is_Valid_Variable_Char(Expr(Pos))) loop
         if not Is_Digit(Expr(Pos)) then
            Is_Number := False;
         end if;
         Pos := Pos + 1;
      end loop;
      
      declare
         Token : constant String := Expr(Start..Pos-1);
      begin
         if Is_Number then
            begin
               Result := Integer'Value(Token);
            exception
               when others =>
                  Error := Syntax_Error;
                  return 0;
            end;
         else
            -- Variable name control
            if not Is_Alpha(Token(Token'First)) then
               Error := Syntax_Error;
               return 0;
            end if;
            
            if Variables.Contains(Token) then
               Result := Variables.Element(Token);
            else
               Error := Undefined_Variable;
               return 0;
            end if;
         end if;
      end;
      
      Skip_Spaces(Expr, Pos);
      return Result;
   end Parse_Number_Or_Variable;
   
   function Parse_Factor(Expr : in String; Pos : in out Integer; Error : out Error_Type) return Integer is
      Result : Integer;
   begin
      Error := No_Error;
      Skip_Spaces(Expr, Pos);
      
      if Pos > Expr'Last then
         Error := Syntax_Error;
         return 0;
      end if;
      
      if Expr(Pos) = '(' then
         Pos := Pos + 1;
         Result := Parse_Expression(Expr, Pos, Error);
         
         if Error /= No_Error then
            return 0;
         end if;
         
         if Pos > Expr'Last or else Expr(Pos) /= ')' then
            Error := Parentheses_Mismatch;
            return 0;
         end if;
         
         Pos := Pos + 1;
         Skip_Spaces(Expr, Pos);
      else
         Result := Parse_Number_Or_Variable(Expr, Pos, Error);
      end if;
      
      return Result;
   end Parse_Factor;
   
   function Parse_Term(Expr : in String; Pos : in out Integer; Error : out Error_Type) return Integer is
      Result : Integer;
      Operator : Character;
   begin
      Error := No_Error;
      Result := Parse_Factor(Expr, Pos, Error);
      
      if Error /= No_Error then
         return 0;
      end if;
      
      loop
         Skip_Spaces(Expr, Pos);
         
         if Pos > Expr'Last then
            exit;
         end if;
         
         Operator := Expr(Pos);
         
         if Operator = '*' or Operator = '/' then
            Pos := Pos + 1;
            declare
               Next_Factor : constant Integer := Parse_Factor(Expr, Pos, Error);
            begin
               if Error /= No_Error then
                  return 0;
               end if;
               
               if Operator = '*' then
                  Result := Result * Next_Factor;
               else
                  if Next_Factor = 0 then
                     Error := Division_By_Zero;
                     return 0;
                  end if;
                  Result := Result / Next_Factor;
               end if;
            end;
         else
            exit;
         end if;
      end loop;
      
      return Result;
   end Parse_Term;
   
   function Parse_Expression(Expr : in String; Pos : in out Integer; Error : out Error_Type) return Integer is
      Result : Integer;
      Operator : Character;
   begin
      Error := No_Error;
      Result := Parse_Term(Expr, Pos, Error);
      
      if Error /= No_Error then
         return 0;
      end if;
      
      loop
         Skip_Spaces(Expr, Pos);
         
         if Pos > Expr'Last then
            exit;
         end if;
         
         Operator := Expr(Pos);
         
         if Operator = '+' or Operator = '-' then
            Pos := Pos + 1;
            declare
               Next_Term : constant Integer := Parse_Term(Expr, Pos, Error);
            begin
               if Error /= No_Error then
                  return 0;
               end if;
               
               if Operator = '+' then
                  Result := Result + Next_Term;
               else
                  Result := Result - Next_Term;
               end if;
            end;
         else
            exit;
         end if;
      end loop;
      
      return Result;
   end Parse_Expression;
   
   function Parse_Assignment(Expr : in String; Error : out Error_Type) return Boolean is
      Pos : Integer := Expr'First;
      Var_End : Integer;
      Var_Name : Unbounded_String;
      Value : Integer;
   begin
      Error := No_Error;
      Skip_Spaces(Expr, Pos);
      
      -- Read variable name
      Var_End := Pos;
      while Var_End <= Expr'Last and then Is_Valid_Variable_Char(Expr(Var_End)) loop
         Var_End := Var_End + 1;
      end loop;
      
      if Var_End = Pos or else not Is_Alpha(Expr(Pos)) then
         Error := Syntax_Error;
         return False;
      end if;
      
      Var_Name := To_Unbounded_String(Expr(Pos..Var_End-1));
      Pos := Var_End;
      Skip_Spaces(Expr, Pos);
      
      if Pos > Expr'Last or else Expr(Pos) /= '=' then
         Error := Syntax_Error;
         return False;
      end if;
      
      Pos := Pos + 1;
      Skip_Spaces(Expr, Pos);
      
      Value := Parse_Expression(Expr, Pos, Error);
      
      if Error /= No_Error then
         return False;
      end if;
      
      if Pos <= Expr'Last then
         Error := Syntax_Error;
         return False;
      end if;
      
      Variables.Include(To_String(Var_Name), Value);
      return True;
   end Parse_Assignment;
   
   function Evaluate(Expr : in String; Error : out Error_Type) return Integer is
      Trimmed_Expr : constant String := Trim(Expr, Both);
      Pos : Integer := Trimmed_Expr'First;
   begin
      Error := No_Error;
      
      -- Input validation
      if not Is_Valid_Input(Trimmed_Expr) then
         Error := Syntax_Error;
         return 0;
      end if;
      
      -- Assignment control
      if Index(Trimmed_Expr, "=") > 0 then
         if Parse_Assignment(Trimmed_Expr, Error) then
            if Variables.Contains(Trimmed_Expr(Trimmed_Expr'First..Index(Trimmed_Expr, "=")-1)) then
               return Variables.Element(Trimmed_Expr(Trimmed_Expr'First..Index(Trimmed_Expr, "=")-1));
            end if;
         end if;
         return 0;
      end if;
      
      -- Normal expression validation
      return Parse_Expression(Trimmed_Expr, Pos, Error);
   end Evaluate;
   
   -- Main program
   Input : String(1..256);
   Length : Natural;
   Result : Integer;
   Error : Error_Type;
begin
   Put_Line("Enter an arithmetic expression - Enter 'q' to quit");
   loop
      Put("> ");
      Flush;
      Get_Line(Input, Length);
      
      if Input(1..Length) = "q" then
         exit;
      end if;
      
      Result := Evaluate(Input(1..Length), Error);
      
      case Error is
         when No_Error =>
            Put("Result: ");
            Put(Result, Width => 1);
            New_Line;
         when Division_By_Zero =>
            Put_Line("Error: Division by zero");
         when Undefined_Variable =>
            Put_Line("Error: Undefined variable");
         when Syntax_Error =>
            Put_Line("Error: Invalid syntax or characters");
         when Parentheses_Mismatch =>
            Put_Line("Error: Mismatched parentheses");
      end case;
   end loop;
   Put_Line("Goodbye!");
end Calculator;
