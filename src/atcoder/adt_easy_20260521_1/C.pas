program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 21;
var
	m, p3: int32;
	e, n, i: int8;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(m);

	p3 := 1;
	e := 0;
	while 3 * p3 <= m do begin
		p3 := 3 * p3;
		inc(e);
	end;

	n := 0;
	while m > 0 do
		if m >= p3 then begin
			inc(n);
			a[n] := e;
			dec(m, p3);
		end else begin
			p3 := p3 div 3;
			dec(e);
		end;

	writeln(n);
	for i := n downto 2 do write(a[i], ' ');
	writeln(a[1]);
end.
