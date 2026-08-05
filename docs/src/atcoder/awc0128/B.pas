program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, k, i, l, r, s, ans: int32;
	w: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	readln(w);

	for i := 2 to n do w[i] := w[2*i-1];

	s := 0;
	ans := 0;
	l := 1;

	for r := 1 to n do begin
		if w[r] = 'S' then inc(s);
		if r-l+1 > k then begin
			if w[l] = 'S' then dec(s);
			inc(l);
		end;
		ans := max(ans, s);
	end;

	writeln(ans);
end.
