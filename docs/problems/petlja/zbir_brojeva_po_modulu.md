# Problem: zbir_brojeva_po_modulu.pas

```pascal
program zbir_brojeva_po_modulu;
var
    n: qword;
    m: int8;

begin
    readln(n);
    readln(m);
    writeln((n+1) * n div 2 mod m);
end.

```
