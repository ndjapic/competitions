program D_Not_Alone;
uses
	math;
const
	nn = 200 * 1000;
	inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
	notc, tci, n, i, mn, mx: int32;
	e: int8;
	ans: int64;
	a: array [0 .. nn] of int32;
	dp: array [0 .. nn] of int64;

function sol(): int64;
var
	i: int32;
begin
	for i := 1 to n do dp[i] := inf;
	dp[0] := 0;

	for i := 1 to n do begin

		if i > 1 then begin
			mn := min(a[i-1], a[i]);
			mx := max(a[i-1], a[i]);
			dp[i] := min(dp[i], dp[i-2] + mx - mn);
		end;

		if i > 2 then begin
			mn := min(a[i-2], mn);
			mx := max(a[i-2], mx);
			dp[i] := min(dp[i], dp[i-3] + mx - mn);
		end;

	end;

	sol := dp[n];
end;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]);
		readln;

		ans := inf;
		for e := 1 to 3 do begin
			ans := min(ans, sol());
			for i := 1 to n do a[i-1] := a[i];
			a[n] := a[0];
		end;

		writeln(ans);

	end;
end.
