program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	b: int8;
	a, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);

	ans := 1;
	while b > 0 do
		if odd(b) then begin
			ans := ans * a;
			dec(b);
		end else begin
			a := sqr(a);
			b := b div 2;
		end;

	writeln(ans);
end.
