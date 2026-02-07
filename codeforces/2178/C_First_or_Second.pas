program C_First_or_Second;
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i: int32;
	ans: int64;
	a: array [1 .. nn] of int32;
	s1, s2: array [0 .. nn] of int64;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		s1[0] := 0;
		s2[0] := 0;
		for i := 1 to n do begin
			read(a[i]);
			s1[i] := s1[i-1] + a[i];
			s2[i] := s2[i-1] + abs(a[i]);
		end;
		readln;

		ans := - (s1[n] - s1[1]);
		for i := 2 to n do
			ans := max(ans, a[1] + (s2[i-1] - s2[1]) - (s1[n] - s1[i]));

		writeln(ans);

	end;
end.
