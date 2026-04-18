program B_Set_of_Strangers;
uses
    math;
const
    nn = 700;
    nnmm = nn * nn;
var
    ntc, tci: int16;
    n, m, nm, i, j, v, c, s, mx: int32;
    a: array [1 .. nnmm] of int32;
    dsu, sz, moves: array [1 .. nnmm] of int32;

function leader(v: int32): int32;
begin
    if dsu[dsu[v]] <> dsu[v] then dsu[v] := leader(dsu[v]);
    leader := dsu[v];
end;

procedure merge(u, v: int32);
begin
    if a[u] = a[v] then begin
        u := leader(u);
        v := leader(v);
        if u <> v then begin
            if sz[u] > sz[v] then begin
                dsu[v] := u;
                inc(sz[u], sz[v]);
            end else begin
                dsu[u] := v;
                inc(sz[v], sz[u]);
            end;
        end;
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, m);
        nm := n*m;

        for v := 1 to nm do begin
            dsu[v] := v;
            sz[v] := 1;
            moves[v] := 0;
        end;

        v := 0;
        for i := 1 to n do begin
            for j := 1 to m do begin
                inc(v);
                read(a[v]);
                if j > 1 then merge(v-1, v);
                if i > 1 then merge(v-m, v);
            end;
            readln;
        end;

        for v := 1 to nm do
            if leader(v) = v then begin
                c := a[v];
                moves[c] := max(moves[c], min(2, sz[v]));
            end;

        s := 0;
        mx := 0;
        for c := 1 to nm do begin
            inc(s, moves[c]);
            mx := max(mx, moves[c]);
        end;

        writeln(s-mx);

    end;
end.
