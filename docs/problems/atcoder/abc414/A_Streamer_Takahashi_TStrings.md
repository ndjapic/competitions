# Problem: A_Streamer_Takahashi_TStrings.pas

```pascal
program A_Streamer_Takahashi_TStrings;
{$H+}{$INLINE ON}
uses
	sysutils, classes;
var
	n, i, l, r, x, y, ans: int32;
	Inputs: TStringList;

// Helper to read a line and split tokens
procedure ParseIn(Inputs: TStrings); inline;
var
	Line : string;
begin
	readln(Line);
	Inputs.DelimitedText := Line;
end;

begin
	Inputs := TStringList.Create;
	Inputs.Clear;
	Inputs.Delimiter := ' ';

	ParseIn(Inputs);

	n := StrToInt(Inputs[0]);
	l := StrToInt(Inputs[1]);
	r := StrToInt(Inputs[2]);

	ans := 0;
	for i := 1 to n do begin

		ParseIn(Inputs);

		x := StrToInt(Inputs[0]);
		y := StrToInt(Inputs[1]);

		if (x <= l) and (r <= y) then inc(ans);

	end;

	writeln(ans);
	flush(StdErr); flush(output); // DO NOT REMOVE

	FreeAndNil(Inputs);
end.

```
