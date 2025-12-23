program F_Greetings;
uses
	math;
const
    maxn = 200 * 1000;
type
    tarr32 = array [1 .. maxn] of int32;
var
	ntc, tci: int16;
    n, i: int32;
    greetings: int64;
    a, b, c, p, merge: tarr32;

procedure msorti(var indices, priority: tarr32; l, r: int32);
var
    m, i, j, k: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msorti(indices, priority, l, m);
        msorti(indices, priority, m+1, r);

        j := l;
        k := m+1;
        for i := l to r do
            if (k > r) or (j <= m) and (
                priority[indices[j]] <= priority[indices[k]]
            ) then begin
                merge[i] := indices[j];
                inc(j);
            end else begin
                merge[i] := indices[k];
                inc(greetings, m-j+1);
                inc(k);
            end;

        for i := l to r do indices[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

		for i := 1 to n do begin
			readln(a[i], b[i]);
			p[i] := i;
		end;

		msorti(p, a, 1, n);

		for i := 1 to n do c[i] := b[p[i]];
		for i := 1 to n do p[i] := i;

		greetings := 0;
		msorti(p, c, 1, n);

		writeln(greetings);

    end;
end.
