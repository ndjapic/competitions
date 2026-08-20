program D_XNOR_Operation;
{$MODE DELPHI}
const
	nn = 200 * 1000;
var
	n, i, dp0, dp1, x: int32;
	ans: int64;
	t: string;

begin
	readln(n);
	readln(t);

	dp0 := 0;
	dp1 := 0;
	ans := 0;

	for i := 1 to n do begin
		case t[i] of

			'0': begin
				x := dp0;
				dp0 := dp1;
				dp1 := x+1;
			end;

			'1': inc(dp0);

		end;
		inc(ans, dp0);
	end;

	writeln(ans);
end.
