program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	notc, tci, n, i, j, k: int32;
	bonus: int8;
	a, b, c: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		read(a[1]);
		b[1] := a[1];
		c[1] := 1;
		k := 1;

		for i := 2 to n do begin
			read(a[i]);
			if a[i] <> a[i-1] then begin
				inc(k);
				b[k] := a[i];
				c[k] := 0;
			end;
			inc(c[k]);
		end;
		readln;

		bonus := 0;

		for j := 1 to k do
			if c[j] > 1 then begin
				if (j+2 <= k) and (b[j+2] <> b[j]) or (j+1 = k) then
					bonus := 1
				else if (j-2 >= 1) and (b[j-2] <> b[j]) or (j-1 = 1) then
					bonus := 1;
			end;

		for j := 2 to k do
			if (c[j-1] > 1) and (c[j] > 1) then bonus := 2;

		writeln(k + bonus);

	end;
end.
