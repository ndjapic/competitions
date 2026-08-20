program D_Match_Mod_Minimize_2;
const
	nn = 300 * 1000;
type
	tarr = array of int32;
var
	ntc, tci, n, m, i, j: int32;
	ans: int64;
	a, b, cp: tarr;
	seen: array of boolean;

procedure msort(var arr: tarr; l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(arr, l, m);
        msort(arr, m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (arr[il] <= arr[ir]) then begin
                cp[i] := arr[il];
                inc(il);
            end else begin
                cp[i] := arr[ir];
                inc(ir);
            end;

        for i := l to r-1 do arr[i] := cp[i];

    end;
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, m);

		setlength(a, n);
		setlength(b, n);
		setlength(cp, n);
		setlength(seen, n);

		for i := 0 to n-1 do read(a[i]); readln; msort(a, 0, n);
		for j := 0 to n-1 do read(b[j]); readln; msort(b, 0, n);
		for i := 0 to n-1 do seen[i] := false;

		j := n-1;
		ans := 0;

		i := 0;
		while i < n do begin
			while (i < n) and (seen[i] or (a[i] + b[j] < m)) do inc(i);
			if i < n then begin
				inc(ans, a[i] + b[j] - m);
				seen[i] := true;
				dec(j);
			end;
		end;

		i := 0;
		while j >= 0 do begin
			while seen[i] do inc(i);
			inc(ans, a[i] + b[j]);
			seen[i] := true;
			dec(j);
		end;

		writeln(ans);

	end;
end.
