program _C1;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	notc, tci, n, i, j, k: int32;
	a, b: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]);
		readln;

		k := 0;

		for i := n downto 1 do begin
			if odd(k) then a[i] := -a[i];
			if a[i] > 0 then begin
				inc(k);
				b[k] := i;
			end;
		end;

		writeln(k);
		for j := 1 to k-1 do write(b[j], ' ');
		if k > 0 then write(b[k]);
		writeln;

	end;
end.
