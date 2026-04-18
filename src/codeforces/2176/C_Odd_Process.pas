program C_Odd_Process;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, k, c, m: int32;
	a: array [1 .. nn] of int32;
	ans: array [1 .. nn] of int64;
	s: TList<int64>;

begin
	randomize;
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		c := 0;
		m := 0;
		s := TList<int64>.Create;
		s.Add(- (int64(1) shl 60));

		for i := 1 to n do begin
			read(a[i]);
			if odd(a[i]) then begin
				m := max(m, a[i]);
			end else begin
				inc(c);
				s.Add(-a[i]);
				s.Exchange(c, random(c+1));
			end;
		end;
		readln;
		s.Sort;

		s[0] := 0;
		for i := 1 to c do s[i] := s[i-1] - s[i];

		for k := 1 to n do
			if n-c = 0 then
				ans[k] := 0
			else if k <= c+1 then
				ans[k] := m + s[k-1]
			else if (k = n) and not odd(n-c) then
				ans[k] := 0
			else
				ans[k] := m + s[c];

		for k := 1 to n-1 do write(ans[k], ' ');
		writeln(ans[n]);

	end;
end.
