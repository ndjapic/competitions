program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 1000 * 1000;
var
	n, q, i, j, tp, p, h, ans: int32;
	a, c: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do begin
		a[i] := i;
		c[i] := 1;
	end;

	ans := 0;
	for j := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				read(p, h);
				dec(c[a[p]]);
				if c[a[p]] = 1 then dec(ans);

				a[p] := h;

				if c[a[p]] = 1 then inc(ans);
				inc(c[a[p]]);
			end;

			2: writeln(ans);

		end;
		readln;
	end;
end.
