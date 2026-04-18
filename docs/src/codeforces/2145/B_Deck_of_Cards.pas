program B_Deck_of_Cards;
{$MODE DELPHI}
const
	nn = 200 * 1000;
var
	notc, tci, n, k, i, l0, r0, l1, r1: int32;
	s, ans: string;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);
		readln(s);

		l0 := 0;
		r0 := 0;
		l1 := 0;
		r1 := 0;

		for i := 1 to k do begin
			case s[i] of

				'0': begin
					inc(l0);
					inc(l1);
				end;

				'1': begin
					inc(r0);
					inc(r1);
				end;

				'2': begin
					inc(l0);
					inc(r1);
				end;

			end;
		end;

		setlength(ans, n);
		for i := 1 to n do
			if k = n then
				ans[i] := '-'
			else if (i-1 < l1) or (n-i < r0) then
				ans[i] := '-'
			else if (i-1 < l0) or (n-i < r1) then
				ans[i] := '?'
			else
				ans[i] := '+';

		writeln(ans);

	end;
end.
