program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 300 * 1000;
var
	n, q, i, x, y, y0: int32;
	tp: int8;
	b: array [1 .. NN] of int32;
	f, cf: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for x := 1 to n do b[x] := 0;

	f[0] := n;
	cf[0] := n;
	for y := 1 to q do begin
		f[y] := 0;
		cf[y] := 0;
	end;

	y0 := 0;
	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(x);
				y := b[x];
				dec(f[y]);
				if f[y0] = 0 then inc(y0);
				inc(y);
				b[x] := y;
				inc(f[y]);
				inc(cf[y]);
			end;

			2: begin
				readln(y);
				inc(y, y0);
				writeln(cf[y])
			end;

		end;
	end;
end.
