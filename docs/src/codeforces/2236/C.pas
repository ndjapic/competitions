program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	notc, tci, a, b, x, ans: int32;
	e: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure swp(var a, b: int32);
var
	s: int32;
begin
	if a < b then begin
		s := a;
		a := b;
		b := s;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(a, b, x);

		swp(a, b);

		e := 0;
		ans := a - b;

		while a > b do begin
			a := a div x;
			swp(a, b);
			inc(e);
			ans := min(ans, a - b + e);
		end;

		writeln(ans);

	end;
end.
