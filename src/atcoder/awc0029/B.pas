program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, q, i, a, b, c: int32;
	tp: int8;
	v: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do read(v[i]);
	readln;

	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(a, b);
				inc(v[b], v[a]);
				v[a] := 0;
			end;

			2: begin
				readln(c);
				writeln(v[c]);
			end;

		end;
	end;
end.
