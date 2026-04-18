program Problem_A_Walk_the_Line;
uses
	Math;
const
	nn = 1000;
var
	ntc, tci, n, i, k, mn: int32;
	ans: boolean;
	s: array [1 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, k);

		mn := high(int32);
		for i := 1 to n do begin
			readln(s[i]);
			mn := min(mn, s[i]);
		end;

		if n = 1 then
			ans := mn <= k
		else
			ans := int64(n-1 + n-2) * mn <= k;

		if ans then
			writeln('Case ', tci, ': YES')
		else
			writeln('Case ', tci, ': NO');

	end;
end.
