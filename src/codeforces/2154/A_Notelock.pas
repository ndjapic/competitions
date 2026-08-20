program A_Notelock;
{$MODE DELPHI}
uses
	math;
const
	nn = 1000;
var
	notc, tci, n, k, i, ans: int32;
	s: string;
	a: array [0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);
		readln(s);

		a[0] := 0;
		ans := 0;
		for i := 1 to n do begin
			if (s[i] = '1') and (a[i-1] - a[max(0, i-k)] = 0) then
				inc(ans);
			a[i] := a[i-1] + ord(s[i]) - ord('0');
		end;

		writeln(ans);

	end;
end.
