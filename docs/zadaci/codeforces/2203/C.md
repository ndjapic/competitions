# Задатак: C.pas

```pascal
program _C;
{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	notc, tci: int32;
	s, m, ans: int64;
	e: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(s, m);

		ans := 0;
		for e := 59 downto 0 do
			if odd(m shr e) then begin
				d := (s+m-1) div m;
				ans := max(ans, s div m);
				s := s div 2;
				m := m div 2;
			end;

		writeln(ans);

	end;
end.

```
