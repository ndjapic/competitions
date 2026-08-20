program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, l, k: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	for i := 1 to n-1 do begin
		l := n-i+1;

		repeat
			dec(l);
			k := 1;
			while (k <= l) and (s[k] <> s[k+i]) do inc(k);
		until k > l;

		writeln(l);
	end;
end.
