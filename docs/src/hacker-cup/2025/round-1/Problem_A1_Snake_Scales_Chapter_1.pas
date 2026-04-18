program Problem_A1_Snake_Scales_Chapter_1;
uses
	math;
const
	nn = 100;
var
	ntc, tci, n, i, ans: int32;
	a: array [1 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		read(a[1]);
		ans := 0;

		for i := 2 to n do begin
			read(a[i]);
			ans := max(ans, abs(a[i] - a[i-1]));
		end;
		readln;

		writeln('Case #', tci, ': ', ans);

	end;
end.
