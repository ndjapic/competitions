# Problem: B_Adjacency_Matrix.pas

```pascal
program B_Adjacency_Matrix;
var
    n, i, j, a: int8;

begin
    readln(n);
    for i := 1 to n do begin
        for j := 1 to n do begin
            read(a);
            if a = 1 then write(j, ' ');
        end;
        readln;
        writeln;
    end;
end.

```
