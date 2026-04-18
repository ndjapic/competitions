# Задатак: G_Shuffling_Songs.pas

```pascal
program G_Shuffling_Songs;
{$H+}
uses
    math;
const
    maxn = 16;
var
    ntc, tci, i, ans: int16;
    m, maxm: word;
    n, u, v, j: int8;
    loop: boolean;
    s: array [1 .. maxn] of string;
    b: array [1 .. maxn] of int16;
    h: array [1 .. maxn] of int8;
    seen: array [1 .. maxn] of word;
    maxlen: array [word] of int8;
    gdsu, wdsu, gsize, wsize: array [1 .. maxn] of int8;
    adj: array [1 .. maxn] of array of int8;

procedure dfs(d, u: int8);
var
    v, j: int8;
    p2: word;
begin
    if d > 0 then begin
        dec(d);
        h[u] := 1;
        p2 := word(1) shl (u-1);
        inc(seen[u], p2);
        for j := 0 to length(adj[u]) - 1 do begin
            v := adj[u][j];
            if seen[v] = 0 then begin
                seen[v] := seen[u];
                dfs(d, v);
                for m := 0 to maxm do
                    if m and seen[u] = 0 then begin
                        maxlen[m or seen[u]] := max(maxlen[m or seen[u]], h[u] + h[v]);
                        ans := max(ans, maxlen[m or seen[u]]);
                    end;
                h[u] := max(h[u], h[v] + 1);
                seen[v] := 0;
            end;
        end;
        dec(seen[u], p2);
        {ans := max(ans, h[u]);}
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        maxm := (int32(1) shl n) - 1;

        for u := 1 to n do begin

            readln(s[u]);
            b[u] := 1;
            while s[u][b[u]] <> ' ' do inc(b[u]);
            gsize[u] := 0;
            wsize[u] := 0;

            v := 1;
            loop := true;
            while (v < u) and loop do begin
                if (gdsu[v] = v) and (b[v] = b[u]) then begin
                    i := 1;
                    while (i < b[v]) and (s[v][i] = s[u][i]) do inc(i);
                    loop := i < b[v];
                end;
                if loop then inc(v);
            end;
            gdsu[u] := v;
            inc(gsize[v]);

            v := 1;
            loop := true;
            while (v < u) and loop do begin
                if (wdsu[v] = v) and (length(s[v]) - b[v] = length(s[u]) - b[u]) then begin
                    i := 1;
                    while (b[v]+i <= length(s[v])) and (s[v][b[v]+i] = s[u][b[u]+i]) do inc(i);
                    loop := b[v]+i <= length(s[v]);
                end;
                if loop then inc(v);
            end;
            wdsu[u] := v;
            inc(wsize[v]);

        end;

        for u := 1 to n do begin
            setlength(adj[u], 0);
            j := 0;
            for v := 1 to n do
                if (gdsu[v] = gdsu[u]) or (wdsu[v] = wdsu[u]) then begin
                    setlength(adj[u], j+1);
                    adj[u][j] := v;
                    inc(j);
                end;
        end;

        ans := 1;
        for u := 1 to n do seen[u] := 0;
        for u := 1 to n do begin
            for m := 0 to maxm do maxlen[m] := 0;
            dfs(n div 2 + 1, u);
        end;
        writeln(n - ans);

    end;
end.

```
