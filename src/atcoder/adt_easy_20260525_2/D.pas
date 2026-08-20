program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, j, k: int8;
	ans: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	ans := 0;
	for j := 2 to n-1 do
		if s[j] = 'B' then begin
			i := j;
			k := j;
			while (1 < i) and (k < n) do begin
				dec(i);
				inc(k);
				if (s[i] = 'A') and (s[k] = 'C') then inc(ans);
			end;
		end;

	writeln(ans);
end.
