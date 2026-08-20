program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 10;
var
	notc, tci, n, i, j, k, w, ans: int32;
	loop: boolean;
	a, b: array [1 .. nn] of int8;
	winner: array [1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do begin
			read(a[i]);
			winner[i] := false;
		end;
		readln;

		ans := 0;
		for i := 1 to n do begin
			for j := 1 to n do b[j] := a[j];
			j := i;
			loop := true;
			while loop do begin
				if b[j] > 0 then w := j;
				dec(b[j]);
				j := j mod n + 1;
				k := 1;
				while (k <= n) and (b[k] <= 0) do inc(k);
				loop := k <= n;
			end;
			if winner[w] = false then inc(ans);
			winner[w] := true;
		end;

		writeln(ans);

	end;
end.
