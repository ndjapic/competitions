program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 1000 * 1000;
var
	n, i, ans: int32;
	a: array [1 .. nn] of int64;
	table: array [1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	read(a[1]);
	table[1] := true;
	ans := n;

	for i := 2 to n do begin
		read(a[i]);
		table[i] := true;

		if table[i-1] then begin
			if a[i-1] > a[i] then begin

				inc(a[i-1], a[i] div 2);
				table[i] := false;
				dec(ans);

			end else if a[i] > a[i-1] then begin

				inc(a[i], a[i-1] div 2);
				table[i-1] := false;
				dec(ans);

			end;
		end;
	end;
	readln;

	writeln(ans);
end.
