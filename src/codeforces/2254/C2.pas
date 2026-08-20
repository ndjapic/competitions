program _C2;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	notc, tci, n, i, ia, ib: int32;
	ans: int64;
	a, b: string;
	sa, sb: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(a);
		readln(b);
		ans := 0;

		if ans >= 0 then begin
			ia := 0;
			ib := 0;

			for i := 1 to n do
				if odd(i) then begin

					if a[i] = '1' then begin
						inc(ia);
						sa[ia] := i;
					end;

					if b[i] = '1' then begin
						inc(ib);
						sb[ib] := i;
					end;

				end;

			if ia <> ib then
				ans := -2
			else
				for i := 1 to ia do
					inc(ans, abs(sa[i] - sb[i]));
		end;

		if ans >= 0 then begin
			ia := 0;
			ib := 0;

			for i := 1 to n do
				if not odd(i) then begin

					if a[i] = '1' then begin
						inc(ia);
						sa[ia] := i;
					end;

					if b[i] = '1' then begin
						inc(ib);
						sb[ib] := i;
					end;

				end;

			if ia <> ib then
				ans := -2
			else
				for i := 1 to ia do
					inc(ans, abs(sa[i] - sb[i]));
		end;

		writeln(ans div 2);

	end;
end.
