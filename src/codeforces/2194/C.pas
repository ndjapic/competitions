program _C;
{$OPTIMIZATION LEVEL3,ON}
const
	nn = 50 * 1000;
var
	notc, tci: int32;
	n, i, j, k, d, x, t: int32;
	ch: char;
	found: boolean;
	ans: string;
	s: array [1 .. nn] of string;
	sieve: array [1 .. nn] of array of int32;
	tau: array [1 .. nn] of int32;
	active: array [1 .. nn, 'a' .. 'z'] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for n := 1 to nn do begin
		setlength(sieve[n], 1);
		sieve[n][0] := 1;
		tau[n] := 1;
	end;

	for d := 2 to nn do begin
		n := d;
		while n <= nn do begin
			if length(sieve[n]) = tau[n] then setlength(sieve[n], 2 * tau[n]);
			sieve[n][tau[n]] := d;
			inc(tau[n]);
			inc(n, d);
		end;
	end; 

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		for i := 1 to n do
			for ch := 'a' to 'z' do active[i, ch] := false;

		for j := 1 to k do begin
			readln(s[j]);
			for i := 1 to n do active[i, s[j][i]] := true;
		end;

		t := 0;
		found := false; (* Search for d. *)
		while (t < tau[n]) and not found do begin

			d := sieve[n][t];
			setlength(ans, d);
			x := 1;
			found := false; (* Search for x. *)

			while (x <= d) and not found do begin
				ans[x] := 'a';
				found := false; (* Search for ans[x]. *)

				while (ans[x] <= 'z') and not found do begin
					i := x;
					while (i <= n) and active[i, ans[x]] do inc(i, d);
					found := i > n; (* ans[x] is found iff failed i is not found. *)
					if not found then inc(ans[x]);
				end;

				found := not found; (* failed x is found iff ans[x] is not found. *)
				inc(x);
			end;

			found := not found; (* d is found iff failed x is not found. *)
			inc(t);
		end;

		for i := 1 to n div d do Write(ans);
		WriteLn;

	end;
end.
