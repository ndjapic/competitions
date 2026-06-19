program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #palindrome #shift
uses
	math;
var
	n, i, j, l, r: int32;
	a, b, cost, ans: int64;
	s: string;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b);
	readln(s);

	ans := int64(1) shl 60;
	for i := 0 to n-1 do begin

		l := 1;
		r := n;
		cost := a * i;
		while l < r do begin
			if s[l] <> s[r] then inc(cost, b);
			inc(l);
			dec(r);
		end;
		ans := min(ans, cost);

		ch := s[1];
		for j := 2 to n do s[j-1] := s[j];
		s[n] := ch;

	end;

	writeln(ans);
end.
