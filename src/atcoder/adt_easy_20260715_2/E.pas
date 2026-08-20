program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int32;
	a, b, c: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	a := 1;
	b := 1;
	c := 1;

	while n > 1 do begin
		if c < b then
			c := 10 * c + 1
		else begin
			c := 1;
			if b < a then
				b := 10 * b + 1
			else begin
				b := 1;
				a := 10 * a + 1;
			end;
		end;
		dec(n);
	end;

	writeln(a + b + c);
end.
