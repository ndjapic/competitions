program E_Modulo_MST;
uses
    math;
const
    maxn = 8;
    maxm = 28;
var
    n, m, i, j, c: int8;
    k, total, ans: int64;
    dsu: array [1 .. maxn] of int8;
    mst: array [1 .. maxn-1] of int8;
    u, v: array [1 .. maxm] of int8;
    w: array [1 .. maxm] of int64;
    pow2: array [0 .. maxn] of int16;

function find(v: int8): int8;
begin
	if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
	find := dsu[v];
end;

procedure union(u, v: int8);
begin
	u := find(u);
	v := find(v);
	if u <> v then begin
		dsu[v] := u;
		dec(c);
	end;
end;

begin
    readln(n, m, k);

    pow2[0] := 1;
    for i := 1 to n do pow2[i] := 2*pow2[i-1];

    for j := 1 to m do readln(u[j], v[j], w[j]);

    for i := 1 to n-1 do mst[i] := i;

    ans := k;
    while mst[n-1] <= m do begin

        c := n;
        total := 0;
		for i := 1 to n do dsu[i] := i;

        for i := 1 to n-1 do begin
            j := mst[i];
            union(u[j], v[j]);
            inc(total, w[j]);
        end;

        if c = 1 then ans := min(ans, total mod k);

        i := 1;
        while (i < n-1) and (mst[i+1] - mst[i] = 1) do begin
            mst[i] := i;
            inc(i);
        end;
        inc(mst[i]);
    end;

    writeln(ans);
end.

