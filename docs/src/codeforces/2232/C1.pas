program _C1;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved
uses
	math;
const
	NN = 3000;
var
	notc, tci, n, i, j, x, s, ans: int32;
	u: string;
	c: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, x, s);
		readln(u);

		for i := 1 to x do c[i] := s;
		ans := 0;

		for j := 1 to n do
			case u[j] of

				'A': begin
					i := 1;
					while (i <= x) and (c[i] = 0) do inc(i);
					if i <= x then begin
						dec(c[i]);
						inc(ans);
					end;
				end;

				'E': begin
					i := 1;
					while (i <= x) and ((c[i] = 0) or (c[i] = s)) do inc(i);
					if i <= x then begin
						dec(c[i]);
						inc(ans);
					end;
				end;

				'I': begin
					i := 1;
					while (i <= x) and (c[i] < s) do inc(i);
					if i <= x then begin
						dec(c[i]);
						inc(ans);
					end;
				end;

			end;

		writeln(ans);

	end;
end.
