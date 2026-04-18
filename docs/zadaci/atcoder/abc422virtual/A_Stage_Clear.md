# Задатак: A_Stage_Clear.pas

```pascal
program A_Stage_Clear;
{$MODE DELPHI}
var
	s: string;
	i, j: int8;

begin
	readln(s);

	i := ord(s[1]) - ord('0');
	j := ord(s[3]) - ord('0');

	inc(j);
	s[3] := chr(ord('0') + j);

	if j > 8 then begin
		inc(i);
		j := 1;
		s[1] := chr(ord('0') + i);
		s[3] := chr(ord('0') + j);
	end;

	writeln(s);
end.

```
