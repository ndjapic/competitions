# Задатак: D_Slavic_s_Exam.pas

```pascal
program D_Slavic_s_Exam;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
	nn = 200 * 1000;
var
    ntc, tci: int16;
    n, m, i, j: int32;
    s, t: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s); n := length(s);
        readln(t); m := length(t);

		j := 1;
		for i := 1 to n do
			if j > m then begin
				if s[i] = '?' then s[i] := 'a';
			end else if s[i] = t[j] then
				inc(j)
			else if s[i] = '?' then begin
				s[i] := t[j];
				inc(j);
			end;

		if j > m then begin
			writeln('YES');
			writeln(s);
		end else
			writeln('NO');

    end;
end.

```
