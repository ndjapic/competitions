# Задатак: C_Striped_Horse.pas

```pascal
program C_Striped_Horse;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 400 * 1000;
var
	notc, tci, n, w, i, ci: int32;
	ans: int64;
	s1, s2: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, w);

		s1[0] := 0;
		s2[0] := 0;
		ans := int64(1) shl 60;

		for i := 1 to n+w do begin

			s1[i] := s1[i-1];
			if i <= n then begin
				read(ci);
				inc(s1[i], ci);
			end;

			s2[i] := s1[i];
			if i > w then begin
				dec(s2[i], s1[i-w]);
				if i > 2*w then inc(s2[i], s2[i-2*w]);
			end;

			if i+w > n then ans := min(ans, s2[i]);

		end;
		readln;

		writeln(ans);

	end;
end.

```
