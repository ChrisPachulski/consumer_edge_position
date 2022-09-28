library(tidyverse)
library(bigrquery)
library(googlesheets4)
library(googledrive)
library(RSelenium)
library(stringr)
library(lubridate)
library(DBI)
#functions#####
#Regex - extract numbers from strings
numextract <- function(string){ 
    str_extract(string, "\\-*\\d+\\.*\\d*")
} 
#Recreating the right & left function from Excel 
right = function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
}
left = function(text, num_char) {
    substr(text, 1, num_char)
}
#Determine Naics Codes
naics_keywords <- function(site_element,naics){
    tryCatch(expr = 
                 {Brand_Desc <- unlist(site_element)
                 naics[i] <- ifelse(grepl("(C|c)lothes", Brand_Desc)==T, "448140",naics[i])
                 naics[i] <- ifelse(grepl("(E|e)scape (R|r)oom", Brand_Desc)==T, "711219",naics[i])
                 naics[i] <- ifelse(grepl("(B|b)usiness", Brand_Desc)==T, "561499",naics[i])
                 naics[i] <- ifelse(grepl("(I|i)nventory", Brand_Desc)==T, "561499",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)andy", Brand_Desc)==T, "445292",naics[i])
                 naics[i] <- ifelse(grepl("(A|a)ttorneys", Brand_Desc)==T, "541110",naics[i])
                 naics[i] <- ifelse(grepl("(L|l)egal", Brand_Desc)==T, "541199",naics[i])
                 naics[i] <- ifelse(grepl("(D|d)ating", Brand_Desc)==T, "812990",naics[i])
                 naics[i] <- ifelse(grepl("(N|n)icotine", Brand_Desc)==T, "453991",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)igar", Brand_Desc)==T, "453991",naics[i])
                 naics[i] <- ifelse(grepl("(V|v)apor\\s*(S|s)tore", Brand_Desc)==T, "453991",naics[i])
                 naics[i] <- ifelse(grepl("(B|b)ed\\s*(B|b)ug", Brand_Desc)==T, "561710",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)offee", Brand_Desc)==T, "722515",naics[i])
                 naics[i] <- ifelse(grepl(" (L|l)oan(s*)", Brand_Desc)==T, "523999",naics[i])
                 naics[i] <- ifelse(grepl("(D|d)ollar", Brand_Desc)==T, "523999",naics[i])
                 naics[i] <- ifelse(grepl("(W|w)ine", Brand_Desc)==T, "445310",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)lower", Brand_Desc)==T, "453110",naics[i])
                 naics[i] <- ifelse(grepl("(N|n)ovelt", Brand_Desc)==T, "453110",naics[i])
                 naics[i] <- ifelse(grepl("(M|m)overs", Brand_Desc)==T, "561990",naics[i])
                 naics[i] <- ifelse(grepl("DNA", Brand_Desc)==T, "561990",naics[i])
                 naics[i] <- ifelse(grepl("(A|a)uto\\s*(P|p)arts", Brand_Desc)==T, "811111",naics[i])
                 naics[i] <- ifelse(grepl("(T|t)ires", Brand_Desc)==T, "811111",naics[i])
                 naics[i] <- ifelse(grepl("(B|b)lood", Brand_Desc)==T, "621999",naics[i])
                 naics[i] <- ifelse(grepl("(^|\\s)(E|e)ye(\\,|\\.|$|\\s)", Brand_Desc)==T, "621999",naics[i])
                 naics[i] <- ifelse(grepl("(H|h)ealth", Brand_Desc)==T, "621999",naics[i])
                 naics[i] <- ifelse(grepl("(L|l)ense", Brand_Desc)==T, "621999",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)linic", Brand_Desc)==T, "621999",naics[i])
                 naics[i] <- ifelse(grepl("(M|m)otorcycle", Brand_Desc)==T, "441228",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)looring", Brand_Desc)==T, "442210",naics[i])
                 naics[i] <- ifelse(grepl("(M|m)attress", Brand_Desc)==T, "442210",naics[i])
                 naics[i] <- ifelse(grepl("(K|k)ids\\s*(C|c)loth", Brand_Desc)==T, "448130",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)tationery", Brand_Desc)==T, "453210",naics[i])
                 naics[i] <- ifelse(grepl("(I|i)nsurance", Brand_Desc)==T, "524210",naics[i])
                 naics[i] <- ifelse(grepl("(E|e)lectrical\\s(S|s)upply", Brand_Desc)==T, "221122",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)wimsuit", Brand_Desc)==T, "448150",naics[i])
                 naics[i] <- ifelse(grepl(" (D|d)og(s*) ", Brand_Desc)==T, "453910",naics[i])
                 naics[i] <- ifelse(grepl(" (P|p)et(s*) ", Brand_Desc)==T, "453910",naics[i])
                 naics[i] <- ifelse(grepl("(A|a)quarium", Brand_Desc)==T, "453910",naics[i])
                 naics[i] <- ifelse(grepl("(B|b)lender", Brand_Desc)==T, "443141",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)iltrat", Brand_Desc)==T, "443141",naics[i])
                 naics[i] <- ifelse(grepl(" (G|g)ifts", Brand_Desc)==T, "453220",naics[i])
                 naics[i] <- ifelse(grepl(" (G|g)od ", Brand_Desc)==T, "453220",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)ruit\\s*(B|b)asket", Brand_Desc)==T, "453220",naics[i])
                 naics[i] <- ifelse(grepl("(G|g)reeting\\s*(C|c)ards", Brand_Desc)==T, "453220",naics[i])
                 naics[i] <- ifelse(grepl("(E|e)-*commerce", Brand_Desc)==T, "454111",naics[i])
                 naics[i] <- ifelse(grepl("(M|m)arketplace", Brand_Desc)==T, "454111",naics[i])
                 naics[i] <- ifelse(grepl("(D|d)eal", Brand_Desc)==T, "454111",naics[i])
                 naics[i] <- ifelse(grepl("(H|h)ealthy", Brand_Desc)==T, "446191",naics[i])
                 naics[i] <- ifelse(grepl(" P3 ", Brand_Desc)==T, "446191",naics[i])
                 naics[i] <- ifelse(grepl("(W|w)eight", Brand_Desc)==T, "446191",naics[i])
                 naics[i] <- ifelse(grepl("(J|j)uice", Brand_Desc)==T, "446191",naics[i])
                 naics[i] <- ifelse(grepl("(N|n)atural\\s*(O|o)ils", Brand_Desc)==T, "446191",naics[i])
                 naics[i] <- ifelse(grepl("(G|g)roup (T|t)rip", Brand_Desc)==T, "561599",naics[i])
                 naics[i] <- ifelse(grepl("(T|t)icket", Brand_Desc)==T, "561599",naics[i])
                 naics[i] <- ifelse(grepl("(T|t)ravel", Brand_Desc)==T, "561599",naics[i])
                 naics[i] <- ifelse(grepl("(T|t)ours", Brand_Desc)==T, "561599",naics[i])
                 naics[i] <- ifelse(grepl("(T|t)ravel", Brand_Desc)==T, "561599",naics[i])
                 naics[i] <- ifelse(grepl("(L|l)awn", Brand_Desc)==T, "444220",naics[i])
                 naics[i] <- ifelse(grepl("(D|d)estination", Brand_Desc)==T, "561599",naics[i])
                 naics[i] <- ifelse(grepl(" (D|d)ata ", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl(" (A|a)pp ", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl(" (W|w)eb ", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl("(V|v)irtual", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl("SSO", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)oftware", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl("(D|d)ownload", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl("(W|w)ebsite", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)yber", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)ocial\\s*(N|n)etwork", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl("(G|g)raphic(s*)", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl("iOS", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl("(N|n)ewsletter", Brand_Desc)==T, "511120",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)acial", Brand_Desc)==T, "511120",naics[i])
                 naics[i] <- ifelse(grepl("(H|h)otel", Brand_Desc)==T, "721110",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)eafood", Brand_Desc)==T, "722513",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)uits", Brand_Desc)==T, "448110",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)light", Brand_Desc)==T, "481111",naics[i])
                 naics[i] <- ifelse(grepl("(A|a)irway", Brand_Desc)==T, "481111",naics[i])
                 naics[i] <- ifelse(grepl("(A|a)irline", Brand_Desc)==T, "481111",naics[i])
                 naics[i] <- ifelse(grepl("(E|e)quipment", Brand_Desc)==T, "444130",naics[i])
                 naics[i] <- ifelse(grepl("(M|m)ignon", Brand_Desc)==T, "445110",naics[i])
                 naics[i] <- ifelse(grepl("(G|g)rocer", Brand_Desc)==T, "445110",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)oods", Brand_Desc)==T, "445110",naics[i])
                 naics[i] <- ifelse(grepl("(G|g)olf", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)amping", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(W|w)ater (G|g)ear", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(T|t)ools", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(Y|y)oga", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)abric", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)omics", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(M|m)edals", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(G|g)ear", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("DIY", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)pray\\s*(P|p)aint", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("NFL", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("MLB", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(B|b)aseball", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("NBA", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(B|b)asketball", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(H|h)ockey", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("MLS", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)occer", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(V|v)inyl", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)irearm", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(B|b)ook(s*)", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(T|t)oys", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)peakers", Brand_Desc)==T, "443142",naics[i])
                 naics[i] <- ifelse(grepl("(H|h)eadphone", Brand_Desc)==T, "443142",naics[i])
                 naics[i] <- ifelse(grepl("(H|h)eadset", Brand_Desc)==T, "443142",naics[i])
                 naics[i] <- ifelse(grepl("HP", Brand_Desc)==T, "443142",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)amera", Brand_Desc)==T, "443142",naics[i])
                 naics[i] <- ifelse(grepl("(P|p)hone", Brand_Desc)==T, "443142",naics[i])
                 naics[i] <- ifelse(grepl(" (W|w)igs ", Brand_Desc)==T, "448190",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)wimwear", Brand_Desc)==T, "448190",naics[i])
                 naics[i] <- ifelse(grepl("(H|h)ats", Brand_Desc)==T, "448190",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)unglass", Brand_Desc)==T, "448190",naics[i])
                 naics[i] <- ifelse(grepl("(L|l)eather", Brand_Desc)==T, "448190",naics[i])
                 naics[i] <- ifelse(grepl("(W|w)atches", Brand_Desc)==T, "448310",naics[i])
                 naics[i] <- ifelse(grepl("(T|t)imepece", Brand_Desc)==T, "448310",naics[i])
                 naics[i] <- ifelse(grepl("(E|e)ducat", Brand_Desc)==T, "611710",naics[i])
                 #naics[i] <- ifelse(grepl("(L|l)earn", Brand_Desc)==T, "611710",naics[i])
                 naics[i] <- ifelse(grepl("(A|a)cademic", Brand_Desc)==T, "611710",naics[i])
                 naics[i] <- ifelse(grepl("(L|l)aundry ", Brand_Desc)==T, "442299",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)anvas", Brand_Desc)==T, "442299",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)rame", Brand_Desc)==T, "442299",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)ustom\\s*(P|p)rint", Brand_Desc)==T, "442299",naics[i])
                 naics[i] <- ifelse(grepl("(G|g)arden", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(P|p)ool ", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(H|h)ardware", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(P|p)repware", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(D|d)esk", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(M|m)attresses", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(B|b)edd", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)leep", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)hutter", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(H|h)ot (T|t)ub", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(Y|y)ard", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(R|r)ugs(\\.|\\s)", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)helv", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)urniture", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)aucet", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(K|k)itchen", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)ofa", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(D|d)uvet", Brand_Desc)==T, "442110",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)hoe", Brand_Desc)==T, "448210",naics[i])
                 naics[i] <- ifelse(grepl("(P|p)erfume", Brand_Desc)==T, "446120",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)omplexion", Brand_Desc)==T, "446120",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)kin", Brand_Desc)==T, "446120",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)have", Brand_Desc)==T, "446120",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)ragrance", Brand_Desc)==T, "446120",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)osmetic", Brand_Desc)==T, "446120",naics[i])
                 naics[i] <- ifelse(grepl("(B|b)eauty", Brand_Desc)==T, "446120",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)kin\\s*(C|c)are", Brand_Desc)==T, "446120",naics[i])
                 naics[i] <- ifelse(grepl("(M|m)akeup", Brand_Desc)==T, "446120",naics[i])
                 naics[i] <- ifelse(grepl("(B|b)ike", Brand_Desc)==T, "451110",naics[i])
                 naics[i] <- ifelse(grepl("(B|b)outique", Brand_Desc)==T, "448120",naics[i])
                 naics[i] <- ifelse(grepl("(G|g)own", Brand_Desc)==T, "448120",naics[i])
                 naics[i] <- ifelse(grepl("(H|h)andbag", Brand_Desc)==T, "448190",naics[i])
                 naics[i] <- ifelse(grepl("(M|m)other", Brand_Desc)==T, "448120",naics[i])
                 naics[i] <- ifelse(grepl("(F|f)ashion", Brand_Desc)==T, "448120",naics[i])
                 naics[i] <- ifelse(grepl("(D|d)resses", Brand_Desc)==T, "448120",naics[i])
                 naics[i] <- ifelse(grepl("(T|t)ights", Brand_Desc)==T, "448120",naics[i])
                 naics[i] <- ifelse(grepl("(L|l)ace", Brand_Desc)==T, "448120",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)ock", Brand_Desc)==T, "448140",naics[i])
                 naics[i] <- ifelse(grepl("(U|u)nderwear", Brand_Desc)==T, "448140",naics[i])
                 naics[i] <- ifelse(grepl("(U|u)niform", Brand_Desc)==T, "448140",naics[i])
                 naics[i] <- ifelse(grepl("(T|t)-*(S|s)hirt", Brand_Desc)==T, "448140",naics[i])
                 naics[i] <- ifelse(grepl(" (L|l)evi", Brand_Desc)==T, "448140",naics[i])
                 naics[i] <- ifelse(grepl("(O|o)utfitter", Brand_Desc)==T, "448140",naics[i])
                 naics[i] <- ifelse((grepl("(C|c)lothing", Brand_Desc)==T), "448140",naics[i])
                 naics[i] <- ifelse((grepl("(S|s)hirt", Brand_Desc)==T), "448140",naics[i])
                 naics[i] <- ifelse(grepl("(A|a)pparel", Brand_Desc)==T, "448140",naics[i])
                 naics[i] <- ifelse(grepl("(W|w)omen.*(M|m)en", Brand_Desc)==T, "448140",naics[i])
                 naics[i] <- ifelse(grepl("(M|m)en.*(W|w)omen", Brand_Desc)==T, "448140",naics[i])
                 naics[i] <- ifelse(grepl("(L|l)acrosse", Brand_Desc)==T, "451110",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)at (L|l)itter", Brand_Desc)==T, "453910",naics[i])
                 naics[i] <- ifelse((grepl("(C|c)ash(\\s|\\.|\\,)", Brand_Desc)==T), "523999",naics[i])
                 naics[i] <- ifelse((grepl("(G|g)arden", Brand_Desc)==T), "444220",naics[i])
                 naics[i] <- ifelse((grepl("(I|i)nvest", Brand_Desc)==T), "523999",naics[i])
                 naics[i] <- ifelse((grepl("(F|f)inanc", Brand_Desc)==T), "523999",naics[i])
                 naics[i] <- ifelse((grepl("(E|e)czema", Brand_Desc)==T), "446120",naics[i])
                 naics[i] <- ifelse((grepl("(V|v)irtual", Brand_Desc)==T), "518210",naics[i])
                 naics[i] <- ifelse((grepl("(T|t)oy(s)*", Brand_Desc)==T), "445120",naics[i])
                 naics[i] <- ifelse((grepl("(L|l)aptop", Brand_Desc)==T), "443142",naics[i])
                 naics[i] <- ifelse((grepl("(E|e)dible", Brand_Desc)==T), "445299",naics[i])
                 naics[i] <- ifelse((grepl("(W|w)allpaper", Brand_Desc)==T), "444120",naics[i])
                 naics[i] <- ifelse((grepl("(P|p)aint", Brand_Desc)==T), "444120",naics[i])
                 naics[i] <- ifelse((grepl("(D|d)eodorant", Brand_Desc)==T), "446199",naics[i])
                 naics[i] <- ifelse((grepl("(D|d)elivery", Brand_Desc)==T), "484121",naics[i])
                 
                 naics[i] <- ifelse((grepl("(B|b)aby", Brand_Desc)==T) & (grepl("(D|d)iaper", Brand_Desc)==T), "448130",naics[i])
                 naics[i] <- ifelse((grepl("(K|k)ids", Brand_Desc)==T) & (grepl("(C|c)lothing", Brand_Desc)==T), "448130",naics[i])
                 naics[i] <- ifelse((grepl("(M|m)ac", Brand_Desc)==T) & (grepl("PC", Brand_Desc)==T), "518210",naics[i])
                 naics[i] <- ifelse((grepl("(H|h)ome", Brand_Desc)==T) & (grepl("(C|c)lean", Brand_Desc)==T), "444130",naics[i])
                 naics[i] <- ifelse((grepl("(C|c)loth", Brand_Desc)==T) & (grepl("(W|w)om(e|a)n", Brand_Desc)==T), "448120",naics[i])
                 naics[i] <- ifelse((grepl("(F|f)ashion", Brand_Desc)==T) & (grepl("(W|w)om(e|a)n", Brand_Desc)==T), "448120",naics[i])
                 naics[i] <- ifelse((grepl("(C|c)ollection", Brand_Desc)==T) & (grepl("(W|w)om(e|a)n", Brand_Desc)==T), "448120",naics[i])
                 naics[i] <- ifelse((grepl("(T|t)ool", Brand_Desc)==T) & (grepl("(B|b)rand", Brand_Desc)==T), "444130",naics[i])
                 naics[i] <- ifelse((grepl("(B|b)luetooth", Brand_Desc)==T) & (grepl("(D|d)evice", Brand_Desc)==T), "443142",naics[i])
                 naics[i] <- ifelse((grepl("(M|m)eal", Brand_Desc)==T) & (grepl("(D|d)elivered", Brand_Desc)==T), "492210",naics[i])
                 naics[i] <- ifelse((grepl("(P|p)rofession", Brand_Desc)==T) & (grepl("(N|n)ail", Brand_Desc)==T), "446120",naics[i])
                 naics[i] <- ifelse((grepl("(N|n)ew", Brand_Desc)==T) & (grepl("(^|\\.|\\s)(C|c)ar\\s", Brand_Desc)==T), "441110",naics[i])
                 naics[i] <- ifelse((grepl("(L|l)uxury", Brand_Desc)==T) & (grepl("(D|d)esigner", Brand_Desc)==T), "448190",naics[i])
                 naics[i] <- ifelse((grepl("(W|w)om(e|a)n", Brand_Desc)==T) & (grepl("(S|s)wimwear", Brand_Desc)==T), "448120",naics[i])
                 naics[i] <- ifelse((grepl("(S|s)ervice", Brand_Desc)==T) & (grepl("(P|p)rovider", Brand_Desc)==T), "561499",naics[i])
                 naics[i] <- ifelse((grepl("(M|m)ovie", Brand_Desc)==T) & (grepl("(C|c)hannel", Brand_Desc)==T), "515210",naics[i])
                 naics[i] <- ifelse((grepl("(C|c)hannel", Brand_Desc)==T) & (grepl("(T|t)(V|v)", Brand_Desc)==T), "515210",naics[i])
                 naics[i] <- ifelse((grepl("(B|b)reaking", Brand_Desc)==T) & (grepl("(N|n)ews", Brand_Desc)==T), "511120",naics[i])
                 naics[i] <- ifelse((grepl("(B|b)aby", Brand_Desc)==T) & (grepl("(C|c)loth", Brand_Desc)==T), "448130",naics[i])
                 naics[i] <- ifelse((grepl("(C|c)ontact", Brand_Desc)==T) & (grepl("(L|l)enses", Brand_Desc)==T), "446130",naics[i])
                 naics[i] <- ifelse((grepl("(T|t)imepiece", Brand_Desc)==T) & (grepl("(F|f)ashion", Brand_Desc)==T), "448190",naics[i])
                 naics[i] <- ifelse((grepl("(S|s)oap", Brand_Desc)==T) & (grepl("(S|s)mell", Brand_Desc)==T), "446120",naics[i])
                 naics[i] <- ifelse((grepl("(A|a)rms", Brand_Desc)==T) & (grepl("(G|g)rip", Brand_Desc)==T), "451110",naics[i])
                 naics[i] <- ifelse((grepl("(S|s)heet", Brand_Desc)==T) & (grepl("(M|m)usic", Brand_Desc)==T), "451140",naics[i])
                 naics[i] <- ifelse((grepl("(J|j)ersey", Brand_Desc)==T) & (grepl("(G|g)ear", Brand_Desc)==T), "451110",naics[i])
                 naics[i] <- ifelse((grepl("(M|m)ountain", Brand_Desc)==T) & (grepl("(B|b)ike", Brand_Desc)==T), "451110",naics[i])
                 naics[i] <- ifelse((grepl("(D|d)rug\\s*(M|m)art", Brand_Desc)==T) & (grepl("(P|p)harmac", Brand_Desc)==T), "446110",naics[i])
                 naics[i] <- ifelse((grepl("(S|h)ower", Brand_Desc)==T) & (grepl("(C|c)add", Brand_Desc)==T), "443141",naics[i])
                 naics[i] <- ifelse((grepl("(T|t)alk", Brand_Desc)==T) & (grepl("(T|t)ext", Brand_Desc)==T), "517312",naics[i])
                 naics[i] <- ifelse((grepl("(B|b)usiness", Brand_Desc)==T) & (grepl("(S|s)tamps", Brand_Desc)==T), "561499",naics[i])
                 naics[i] <- ifelse((grepl("(P|p)romotion", Brand_Desc)==T) & (grepl("(M|m)arketing", Brand_Desc)==T), "561499",naics[i])
                 naics[i] <- ifelse((grepl("(F|f)urnish", Brand_Desc)==T) & (grepl("(D|d)ecor", Brand_Desc)==T), "442110",naics[i])
                 naics[i] <- ifelse((grepl("(B|b)iotic", Brand_Desc)==T) & (grepl("(O|o)mega", Brand_Desc)==T), "446191",naics[i])
                 naics[i] <- ifelse((grepl("(T|t)ools", Brand_Desc)==T) & ((grepl("(E|e)yewear", Brand_Desc)==T) |(grepl("(P|p)ower", Brand_Desc)==T) ), "444130",naics[i])
                 naics[i] <- ifelse((grepl("(V|v)eteran", Brand_Desc)==T) & (grepl("(D|d)iscount", Brand_Desc)==T), "524210",naics[i])
                 naics[i] <- ifelse((grepl("(D|d)igital", Brand_Desc)==T) & (grepl("(S|s)tream", Brand_Desc)==T), "515210",naics[i])
                 naics[i] <- ifelse((grepl("(G|g)lass", Brand_Desc)==T) & (grepl("(L|l)ens", Brand_Desc)==T), "446130",naics[i])
                 naics[i] <- ifelse((grepl("(M|m)eal", Brand_Desc)==T) & (grepl("(D|d)eliver", Brand_Desc)==T), "492210",naics[i])
                 naics[i] <- ifelse((grepl("(A|a)ndroid", Brand_Desc)==T) & (grepl("PC", Brand_Desc)==T), "518210",naics[i])
                 naics[i] <- ifelse((grepl("(H|h)air", Brand_Desc)==T) & (grepl("(C|c)are", Brand_Desc)==T), "446120",naics[i])
                 naics[i] <- ifelse((grepl("(O|o)ffice", Brand_Desc)==T) & (grepl("(S|s)uppl", Brand_Desc)==T), "453210",naics[i])
                 naics[i] <- ifelse((grepl("(J|j)acket", Brand_Desc)==T) & (grepl("(S|s)hirt", Brand_Desc)==T), "448140",naics[i])
                 naics[i] <- ifelse((grepl("(B|b)oot", Brand_Desc)==T) & (grepl("(O|o)uterwear", Brand_Desc)==T), "448140",naics[i])
                 naics[i] <- ifelse((grepl("(D|d)esigner", Brand_Desc)==T) & (grepl("(C|c)lothe", Brand_Desc)==T), "448140",naics[i])
                 naics[i] <- ifelse((grepl("(H|h)andbag", Brand_Desc)==T) & (grepl("(C|c)lothe", Brand_Desc)==T), "448120",naics[i])
                 naics[i] <- ifelse((grepl("(H|h)eart", Brand_Desc)==T) & (grepl("(R|r)ate", Brand_Desc)==T), "621999",naics[i])
                 naics[i] <- ifelse((grepl("(G|g)ift", Brand_Desc)==T) & (grepl("(C|c)ard", Brand_Desc)==T), "453220",naics[i])
                 naics[i] <- ifelse((grepl("(C|c)oin", Brand_Desc)==T) & (grepl("(C|c)olllect", Brand_Desc)==T), "451120",naics[i])
                 naics[i] <- ifelse((grepl("(G|g)arden", Brand_Desc)==T) & (grepl("(T|t)ool", Brand_Desc)==T), "444130",naics[i])
                 naics[i] <- ifelse((grepl("(T|t)ravel", Brand_Desc)==T) & (grepl("(L|l)uggage", Brand_Desc)==T), "448320",naics[i])
                 naics[i] <- ifelse((grepl("(S|s)occer", Brand_Desc)==T) & (grepl("(B|b)all", Brand_Desc)==T), "451120",naics[i])
                 naics[i] <- ifelse((grepl("(P|p)lanner", Brand_Desc)==T) & (grepl("(P|p)ens", Brand_Desc)==T), "453210",naics[i])
                 naics[i] <- ifelse((grepl("(P|p)encil", Brand_Desc)==T) & (grepl("(P|p)ens", Brand_Desc)==T), "453210",naics[i])
                 naics[i] <- ifelse((grepl("(L|l)ose", Brand_Desc)==T) & (grepl("(P|p)ound", Brand_Desc)==T), "446191",naics[i])
                 naics[i] <- ifelse((grepl("(I|i)mprov", Brand_Desc)==T) & (grepl("(B|b)ody", Brand_Desc)==T), "446191",naics[i])
                 naics[i] <- ifelse((grepl("(P|p)rotein", Brand_Desc)==T) & (grepl("(S|s)hakes", Brand_Desc)==T), "446191",naics[i])
                 naics[i] <- ifelse((grepl("(V|v)et", Brand_Desc)==T) & (grepl("(C|c)are", Brand_Desc)==T), "541940",naics[i])
                 naics[i] <- ifelse((grepl("(S|s)ingles", Brand_Desc)==T) & (grepl("(M|m)eet", Brand_Desc)==T), "812990",naics[i])
                 naics[i] <- ifelse((grepl("(S|s)hirt", Brand_Desc)==T) & (grepl("(S|s)hoe", Brand_Desc)==T), "448140",naics[i])
                 naics[i] <- ifelse((grepl("(D|d)esigner", Brand_Desc)==T) & (grepl("(B|b)rand", Brand_Desc)==T), "448140",naics[i])
                 naics[i] <- ifelse((grepl("(S|s)kin", Brand_Desc)==T) & (grepl("(F|f)acial", Brand_Desc)==T), "446120",naics[i])
                 naics[i] <- ifelse((grepl("(S|s)calp", Brand_Desc)==T) & (grepl("(H|h)air", Brand_Desc)==T), "446120",naics[i])
                 naics[i] <- ifelse((grepl("(K|k)ids", Brand_Desc)==T) & (grepl("(T|t)oys", Brand_Desc)==T), "611710",naics[i])
                 naics[i] <- ifelse((grepl("(O|o)rganic", Brand_Desc)==T) & (grepl("(P|p)ills", Brand_Desc)==T), "446191",naics[i])
                 naics[i] <- ifelse((grepl("(J|j)eans", Brand_Desc)==T) & (grepl("(W|w)om(a|e)n", Brand_Desc)==T), "448120",naics[i])
                 naics[i] <- ifelse((grepl("(C|c)lothing", Brand_Desc)==T) & (grepl("(A|a)ccesor", Brand_Desc)==T), "448190",naics[i])
                 naics[i] <- ifelse((grepl("(F|f)ruit", Brand_Desc)==T) & (grepl("(V|v)eg", Brand_Desc)==T), "445110",naics[i])
                 naics[i] <- ifelse((grepl("(H|h)otel", Brand_Desc)==T) & (grepl("(B|b)ooking", Brand_Desc)==T), "561599",naics[i])
                 naics[i] <- ifelse((grepl("(R|r)oad", Brand_Desc)==T) & (grepl("(T|t)rip", Brand_Desc)==T), "561599",naics[i])
                 naics[i] <- ifelse((grepl("(V|v)acation", Brand_Desc)==T) & (grepl("(P|p)lan", Brand_Desc)==T), "561599",naics[i])
                 naics[i] <- ifelse((grepl("(F|f)reeze", Brand_Desc)==T) & (grepl("(D|d)ried", Brand_Desc)==T), "722515",naics[i])
                 naics[i] <- ifelse((grepl("(G|g)ame", Brand_Desc)==T) & (grepl("(B|b)all", Brand_Desc)==T), "451120",naics[i])
                 naics[i] <- ifelse((grepl("(G|g)aming", Brand_Desc)==T) & (grepl("(M|m)erch", Brand_Desc)==T), "451120",naics[i])
                 naics[i] <- ifelse((grepl("(M|m)en's", Brand_Desc)==T) & (grepl("(C|l)othes", Brand_Desc)==T), "448110",naics[i])
                 naics[i] <- ifelse((grepl("(R|r)eal", Brand_Desc)==T) & (grepl("(W|w)ood", Brand_Desc)==T), "442110",naics[i])
                 naics[i] <- ifelse((grepl("(W|w)igs", Brand_Desc)==T) & (grepl("(H|h)airpeice", Brand_Desc)==T), "448190",naics[i])
                 naics[i] <- ifelse((grepl("(C|c)harger", Brand_Desc)==T) & (grepl("(C|c)able", Brand_Desc)==T), "443142",naics[i])
                 naics[i] <- ifelse((grepl("(A|a)uto", Brand_Desc)==T) & (grepl("(R|r)epair", Brand_Desc)==T), "811111",naics[i])
                 naics[i] <- ifelse((grepl("(B|b)ooking", Brand_Desc)==T) & (grepl("(T|t)ool", Brand_Desc)==T), "561599",naics[i])
                 naics[i] <- ifelse((grepl("(A|a)pp", Brand_Desc)==T) & (grepl("(M|m)obile (D|d)evice", Brand_Desc)==T), "518210",naics[i])
                 naics[i] <- ifelse((grepl("^(O|o)nline", Brand_Desc)==T) & (grepl("(P|p)rinting", Brand_Desc)==T), "518210",naics[i])
                 naics[i] <- ifelse((grepl("(V|v)acation", Brand_Desc)==T) & (grepl("(R|r)esort", Brand_Desc)==T), "721110",naics[i])
                 naics[i] <- ifelse((grepl("(G|g)ym", Brand_Desc)==T) & (grepl("(E|e)quip", Brand_Desc)==T), "451110",naics[i])
                 naics[i] <- ifelse((grepl("(C|c)ities", Brand_Desc)==T) & (grepl("(A|a)irport", Brand_Desc)==T), "561599",naics[i])
                 naics[i] <- ifelse((grepl("(C|c)oupon", Brand_Desc)==T) & (grepl("(C|c)ode", Brand_Desc)==T), "561990",naics[i])
                 naics[i] <- ifelse((grepl("(W|w)atch", Brand_Desc)==T) & (grepl("(O|o)n(\\-|\\s)(D|d)emand", Brand_Desc)==T), "515210",naics[i])
                 naics[i] <- ifelse((grepl("(T|t)ech", Brand_Desc)==T) & (grepl("(R|r)efurb", Brand_Desc)==T), "443142",naics[i])
                 naics[i] <- ifelse((grepl("(S|s)team", Brand_Desc)==T) & (grepl("PSN", Brand_Desc)==T), "518210",naics[i])
                 naics[i] <- ifelse((grepl("(S|s)hop", Brand_Desc)==T) & (grepl("(T|t)ools", Brand_Desc)==T), "444130",naics[i])
                 naics[i] <- ifelse((grepl("(H|h)ome", Brand_Desc)==T) & (grepl("(F|f)urnish", Brand_Desc)==T), "442110",naics[i])
                 naics[i] <- ifelse((grepl("(E|e)xam", Brand_Desc)==T) & (grepl("(T|t)est", Brand_Desc)==T), "611710",naics[i])
                 naics[i] <- ifelse((grepl("(G|g)ift", Brand_Desc)==T) & (grepl("(F|f)ruit", Brand_Desc)==T), "445299",naics[i])
                 naics[i] <- ifelse((grepl("(L|l)apel", Brand_Desc)==T) & (grepl("(T|t)ies", Brand_Desc)==T), "448190",naics[i])
                 naics[i] <- ifelse((grepl("(B|b)ag", Brand_Desc)==T) & (grepl("(T|t)ravel", Brand_Desc)==T), "448320",naics[i])
                 
                 naics[i] <- ifelse((grepl("(H|h)ealth", Brand_Desc)==T) & (grepl("(H|h)air", Brand_Desc)==T) & (grepl("(S|s)kin", Brand_Desc)==T), "446120",naics[i])
                 naics[i] <- ifelse((grepl("(M|m)en", Brand_Desc)==T) & (grepl("(W|w)omen", Brand_Desc)==T) & (grepl("(K|k)id", Brand_Desc)==T), "448140",naics[i])
                 naics[i] <- ifelse((grepl("(O|o)fficial", Brand_Desc)==T) & (grepl("(G|g)ear", Brand_Desc)==T) & (grepl("(A|a)pparel", Brand_Desc)==T), "451110",naics[i])
                 naics[i] <- ifelse((grepl("(B|b)louses", Brand_Desc)==T) & ((grepl("(C|c)rop", Brand_Desc)==T) | (grepl("(D|d)ress", Brand_Desc)==T)), "448120",naics[i])
                 naics[i] <- ifelse((grepl("(G|g)ift", Brand_Desc)==T) & ((grepl("(C|c)ustom", Brand_Desc)==T) | (grepl("(P|p)ersonal", Brand_Desc)==T)| (grepl("(P|p)aper", Brand_Desc)==T)| (grepl("(C|c)ard", Brand_Desc)==T)), "453220",naics[i])
                 naics[i] <- ifelse((grepl("(T|t)ax", Brand_Desc)==T) & ((grepl("(F|f)iling", Brand_Desc)==T) | (grepl("(R|r)eturn", Brand_Desc)==T)), "541213",naics[i])
                 naics[i] <- ifelse((grepl("(M|m)en", Brand_Desc)==T) & ((grepl("(B|b)oy", Brand_Desc)==T) | (grepl("(S|s)hoe", Brand_Desc)==T)), "448210",naics[i])
                 naics[i] <- ifelse((grepl("(N|n)atural", Brand_Desc)==T) & ((grepl("(O|o)il", Brand_Desc)==T) | (grepl("(I|i)ngredient", Brand_Desc)==T)), "446191",naics[i])
                 
                 
                 naics[i] <- ifelse(grepl("(C|c)ufflink", Brand_Desc)==T, "448110",naics[i])
                 naics[i] <- ifelse(grepl("(D|d)enim", Brand_Desc)==T, "448140",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)tationary", Brand_Desc)==T, "453210",naics[i])
                 naics[i] <- ifelse(grepl("(C|c)artridge", Brand_Desc)==T, "453210",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)kateboard", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)pandex", Brand_Desc)==T, "448140",naics[i])   
                 naics[i] <- ifelse(grepl("CBD ", Brand_Desc)==T, "453998",naics[i])
                 naics[i] <- ifelse(grepl("NHL", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(J|j)ewelry", Brand_Desc)==T, "448310",naics[i])
                 naics[i] <- ifelse(grepl("(P|p)uzzle", Brand_Desc)==T, "451120",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)oftware", Brand_Desc)==T, "518210",naics[i])
                 naics[i] <- ifelse(grepl("(S|s)upplement", Brand_Desc)==T, "446191",naics[i])
                 naics[i] <- ifelse(grepl(" (L|l)ease ", Brand_Desc)==T, "523999",naics[i])
                 naics[i] <- ifelse((grepl("(F|f)lashlight", Brand_Desc)==T), "443142",naics[i])
                 naics[i] <- ifelse((grepl("(L|l)ip (B|b)alm", Brand_Desc)==T), "446199",naics[i])
                 
                 naics[i]
                 }, error = function(e){})
}
#Remove/alter unwanted naics (Wholesalers, Manufacturers)
naic_to_naic <- function(naics){
    naics <- ifelse(naics == "316210", "448210", naics)
    naics <- ifelse(naics == "541715", "621999", naics)
    naics <- ifelse(naics == "311119", "446191", naics)
    naics <- ifelse(naics == "424320", "442110", naics)
    naics <- ifelse(naics == "315190", "448140", naics)
    naics <- ifelse(naics == "315190", "448140", naics)
    naics <- ifelse(naics == "339115", "446130", naics)
    naics <- ifelse(naics == "423220", "448140", naics)
    naics <- ifelse(naics == "334220", "517919", naics)
    naics <- ifelse(naics == "332994", "451120", naics)
    naics <- ifelse(naics == "337215", "442110", naics)
    naics <- ifelse(naics == "315990", "448190", naics)
    naics <- ifelse(naics == "423140", "441310", naics)
    naics <- ifelse(naics == "811412", "443141", naics)
    naics
}
#Overrides - Certain Brand names are highly definitive
brand_to_naic_overrides <- function(naics){
    naics <- ifelse(grepl("caesars", initial_search$brand_url_addr)==T, "721120", naics)
    naics <- ifelse(grepl("lingerie", initial_search$brand_url_addr)==T, "448120", naics)
    naics <- ifelse(grepl("museum", initial_search$brand_url_addr)==T, "712110", naics)
    naics <- ifelse(grepl("furniture", initial_search$brand_url_addr)==T, "442110", naics)
    naics <- ifelse(grepl("blinds", initial_search$brand_url_addr)==T, "442110", naics)
    naics <- ifelse(grepl("moneytransfer", initial_search$brand_url_addr)==T, "523999", naics)
    naics <- ifelse(grepl("skincare", initial_search$brand_url_addr)==T, "446120", naics)
    naics <- ifelse(grepl("hotels", initial_search$brand_url_addr)==T, "721110", naics)
    naics <- ifelse(grepl("diet", initial_search$brand_url_addr)==T, "446191", naics)
    
    naics <- ifelse(grepl(" MASK", initial_search$brand)==T, "448190", naics)
    naics <- ifelse(grepl("( BEER|BEER )", initial_search$brand)==T, "445310", naics)
    naics <- ifelse(grepl("BOX", initial_search$brand)==T, "451120", naics)
    naics <- ifelse(grepl(" CRATE", initial_search$brand)==T, "451120", naics)
    naics <- ifelse(grepl(" OVEN", initial_search$brand)==T, "443141", naics)
    naics <- ifelse(grepl("TRAVEL", initial_search$brand)==T, "561510", naics)
    naics <- ifelse(grepl("LAS VEGAS", initial_search$brand)==T, "721120", naics)
    naics <- ifelse(grepl("LIGHTING", initial_search$brand)==T, "442110", naics)
    naics <- ifelse(grepl("BULLION", initial_search$brand)==T, "451120", naics)
    naics <- ifelse(grepl("COLLECTION", initial_search$brand)==T, "448120", naics)
    naics <- ifelse(grepl("COSMETIC", initial_search$brand)==T, "446120", naics)
    naics <- ifelse(grepl("BEAUTY", initial_search$brand)==T, "446120", naics)
    naics <- ifelse(grepl("SHIRTS", initial_search$brand)==T, "448140", naics)
    naics <- ifelse(grepl("PROM", initial_search$brand)==T, "448120", naics)
    naics <- ifelse(grepl("AUTOPART", initial_search$brand)==T, "811111", naics)
    naics <- ifelse(grepl("MOTOR", initial_search$brand)==T, "811111", naics)
    naics <- ifelse(grepl("SECURITY", initial_search$brand)==T, "561621", naics)
    naics <- ifelse(grepl("DR\\s", initial_search$brand)==T, "621999", naics)
    naics <- ifelse(grepl("COSTUME", initial_search$brand)==T, "448150", naics)
    naics <- ifelse(grepl("WINE", initial_search$brand)==T, "445310", naics)
    naics <- ifelse(grepl("MEMORABILIA", initial_search$brand)==T, "451120", naics)
    naics <- ifelse(grepl("ON DEMAND", initial_search$brand)==T, "515210", naics)
    naics <- ifelse(grepl("SCHOOL SUPPLY", initial_search$brand)==T, "453210", naics)
    naics <- ifelse(grepl("BEDDING", initial_search$brand)==T, "442110", naics)
    naics <- ifelse(grepl("^TEA ", initial_search$brand)==T, "445299", naics)
    naics <- ifelse(grepl("GAME PASS", initial_search$brand)==T, "515210", naics)
    naics <- ifelse(grepl("HEMP", initial_search$brand)==T, "446191", naics)
    naics <- ifelse(grepl("FILTERS", initial_search$brand)==T, "811412", naics)
    naics <- ifelse(grepl("JEWELRY", initial_search$brand)==T, "448310", naics)
    naics <- ifelse(grepl("^DR\\s", initial_search$brand)==T, "621999", naics)
    naics <- ifelse(grepl("APPAREL", initial_search$brand)==T, "448140", naics)
    naics <- ifelse(grepl("ECZEMA", initial_search$brand)==T, "446120", naics)
    naics <- ifelse(grepl("FOUNDATION", initial_search$brand)==T, "813219", naics)
    naics <- ifelse(grepl("BOARD", initial_search$brand)==T, "451120", naics)
    naics <- ifelse(grepl("REAL ESTATE", initial_search$brand)==T, "531390", naics)
    naics <- ifelse(grepl("CLOTHING", initial_search$brand)==T, "448140", naics)
    naics <- ifelse(grepl(" PRESS$", initial_search$brand)==T, "511120", naics)
    naics <- ifelse(grepl("PUBLISHING", initial_search$brand)==T, "511120", naics)
    
    naics
}
#Miscellaneous Naics - assign them to desired subindustries
subind_to_naic_overrides <- function(subind){
  subind  <- ifelse(grepl("Hardware",initial_search$info)==T,1014, subind )
  subind  <- ifelse((grepl("SPORT",initial_search$brand)==T)&(grepl("448",initial_search$naics)),1002, subind )
  subind  <- ifelse((grepl("CBD",initial_search$brand)==T),1020, subind )
  subind  <- ifelse((grepl("(^|\\s)DECOR(\\s|$)",initial_search$brand)==T),1014, subind )
  #subind  <- ifelse( subind  == 1005,1000, subind )
 
  subind  <- ifelse(initial_search$naics == 448140,1000, subind )
  subind  <- ifelse(initial_search$naics == 492210,1024, subind )
  subind  <- ifelse(initial_search$naics == 446130,1031, subind )
  subind  <- ifelse(initial_search$naics == 511120,1037, subind )
  subind  <- ifelse(initial_search$naics == 517312,1037, subind )
  subind  <- ifelse(initial_search$naics == 811412,1047, subind )
  subind  <- ifelse(initial_search$naics == 812990,1047, subind )
  subind  <- ifelse(initial_search$naics == 561990,1047, subind )
  subind  <- ifelse(initial_search$naics == 488210,1042, subind )
  subind  <- ifelse(initial_search$naics == 485510,1042, subind )
  subind  <- ifelse(initial_search$naics == 611692,1028, subind )
  subind  <- ifelse(initial_search$naics == 423140,1010, subind )
  subind  <- ifelse(initial_search$naics == 518210,1029, subind )
  subind  <- ifelse(initial_search$naics == 448150,1005, subind )
  subind  <- ifelse(initial_search$naics == 445299,1027, subind )
  subind  <- ifelse(initial_search$naics == 443141,1014, subind )
  subind  <- ifelse(initial_search$naics == 561621,1013, subind )
  subind  <- ifelse(initial_search$naics == 518210,1029, subind )
  subind  <- ifelse(initial_search$naics == 445299,1024, subind )
  subind  <- ifelse(initial_search$naics == 453991,1020, subind )
  subind  <- ifelse(initial_search$naics == 561499,1008, subind )
  subind  <- ifelse(initial_search$naics == 448320,1004, subind )
  subind  <- ifelse(initial_search$naics == 712110,1043, subind )
  subind  <- ifelse(initial_search$naics == 561499,1008, subind )
  subind  <- ifelse(initial_search$naics == 518210,1029, subind )
  subind  <- ifelse(initial_search$naics == 442210,1014, subind )

  subind
}
#Initial Naics discernment
chrome <-function(ip){
  remDr = remoteDriver(remoteServerAddr = ip, port = 4445L, browser = "chrome")
  remDr$open()
  remDr$maxWindowSize()
  remDr
}
#Create a Selenium Session that auto log into CEI Internals
CEI_Login <- function(ip,email,password){
  remDr = remoteDriver(remoteServerAddr = ip, port = 4445L, browser = "chrome")
  remDr$open()
  remDr$maxWindowSize()
  remDr$navigate("https://internals.consumer-edge.com/auth/login?next=%2Fsymbol%2Flist")
  Sys.sleep(2)
  username <- remDr$findElement(using = "id", value = "username")
  username$clearElement()
  username$sendKeysToElement(list(email))
  
  passwd <- remDr$findElement(using = "id", value = "password")
  passwd$clearElement()
  passwd$sendKeysToElement(list(password))
  
  Post_Credential_Login <- remDr$findElement(using = "id", value = "submit")
  Post_Credential_Login$submitElement()
  Sys.sleep(2)
  remDr
}
#Key Initial Search
initial_google <- function(initial_search){
  remDr$navigate("https://google.com")
  Sys.sleep(3)
  webElem <- remDr$findElement(using = 'name', value = "q")
  webElem$sendKeysToElement(list(initial_search$brand[i], key = "enter"))
  Sys.sleep(3)
  initial_search$naics[i] <- unlist(if(length(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[2]/div/span')$getElementText(),initial_search$naics))==0){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[2]/div/span')$getElementText(),initial_search$naics)})
  
  if(is.na(initial_search$naics[i]) == T){
    initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div[2]/div/span')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div[2]/div/span')$getElementText(),initial_search$naics)}
  }
  Sys.sleep(.5)
  if(is.na(initial_search$naics[i]) == T){
    initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[4]/div/div[2]/div/span')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[4]/div/div[2]/div/span')$getElementText(),initial_search$naics)}
  }
  Sys.sleep(.5)
  if(is.na(initial_search$naics[i]) == T){
    initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[5]/div/div[2]/div/span')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[5]/div/div[2]/div/span')$getElementText(),initial_search$naics)}
  }
  Sys.sleep(.5)
  if(is.na(initial_search$naics[i]) == T){
    initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/table/tbody/tr[1]/td[1]/div/div/div')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/table/tbody/tr[1]/td[1]/div/div/div')$getElementText(),initial_search$naics)}
  }
  if(is.na(initial_search$naics[i]) == T){
    initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[1]/a/h3')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[1]/a/h3')$getElementText(),initial_search$naics)}
  }
  Sys.sleep(.5)
  if(is.na(initial_search$naics[i]) == T){
    initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[2]/div/span')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[2]/div/span')$getElementText(),initial_search$naics)}
  }
  Sys.sleep(.5)
  if(is.na(initial_search$naics[i]) == T){
    initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[4]/div/div[1]/a/h3')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[4]/div/div[1]/a/h3')$getElementText(),initial_search$naics)}
  }
  
  
  
  # Should brand name not be enough to discern, attempt to use the same criteria above, but this time utilizing
  # the brands URL, it has proven to be a helpful catch
  
  if(is.na(initial_search$naics[i]) == T){
    remDr$navigate("https://google.com")
    Sys.sleep(2)
    webElem <- remDr$findElement(using = 'name', value = "q")
    webElem$sendKeysToElement(list(gsub("https:\\/\\/","",gsub(".*www\\.","",initial_search$brand_url_addr[i])), key = "enter"))
    Sys.sleep(2)
    if(is.na(initial_search$naics[i]) == T){
      initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[2]/div/span')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[2]/div/span')$getElementText(),initial_search$naics)}
    }
    if(is.na(initial_search$naics[i]) == T){
      initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div[2]/div/span')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div[2]/div/span')$getElementText(),initial_search$naics)}
    }
    Sys.sleep(.5)
    if(is.na(initial_search$naics[i]) == T){
      initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[4]/div/div[2]/div/span')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[4]/div/div[2]/div/span')$getElementText(),initial_search$naics)}
    }
    Sys.sleep(.5)
    if(is.na(initial_search$naics[i]) == T){
      initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[5]/div/div[2]/div/span')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[5]/div/div[2]/div/span')$getElementText(),initial_search$naics)}
    }
    Sys.sleep(.5)
    if(is.na(initial_search$naics[i]) == T){
      initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/table/tbody/tr[1]/td[1]/div/div/div')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/table/tbody/tr[1]/td[1]/div/div/div')$getElementText(),initial_search$naics)}
    }
    if(is.na(initial_search$naics[i]) == T){
      initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[1]/a/h3')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[1]/a/h3')$getElementText(),initial_search$naics)}
    }
    Sys.sleep(.5)
    if(is.na(initial_search$naics[i]) == T){
      initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[2]/div/span')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[2]/div/span')$getElementText(),initial_search$naics)}
    }
    Sys.sleep(.5)
    if(is.na(initial_search$naics[i]) == T){
      initial_search$naics[i] <- if(is.null(naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[2]/div/div[1]/a/h3')$getElementText(),initial_search$naics))==T){NA}else{naics_keywords(remDr$findElement('xpath','//*[@id="rso"]/div[1]/div/div/div[2]/div/span')$getElementText(),initial_search$naics)}
    }
    
  }
  
  initial_search$naics[i]
}
#Unique Naics
cei_naics_bq_call <- function(email){
  bq_auth(email = email, use_oob = TRUE)
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "consumeredgeresearch",
    dataset = "store_location.OC_algo_QA",
    billing = "consumeredgeresearch"
  )
  
  #Fetch the NAICS descriptions within our own tables for easy human interpretation/review 
  statement <- paste(
    "with t1 as (SELECT DISTINCT naics_id, naics_description FROM `cei-data-science.sandbox.mcc_to_naics`), ",
    "t2 as (SELECT distinct(naics_code), count(brand_name) as brand_count FROM `consumeredgeresearch.apollo_ground_truth.brand` ",
    "group by 1 ",
    "order by brand_count desc) ",
    "SELECT naics_id, naics_description, brand_count ",
    "FROM t1  ",
    "left join t2 on naics_id = naics_code ",
    "order by brand_count desc ",
    sep = ""
  )
  rs <- dbSendQuery(con, statement = statement)
  
  # we now fetch records from the resultSet into a data.frame
  data <- NULL
  data <- dbFetch(rs, n = -1)   # extract all rows
  cei_naics <- as.data.frame(data)
  cei_naics
}
#Subindustries and Naics of existing CEI BU
cei_subind_IDs_bq_call <- function(email){
  bq_auth(email = email, use_oob = TRUE)
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "consumeredgeresearch",
    dataset = "store_location.OC_algo_QA",
    billing = "consumeredgeresearch"
  )
  statement <- paste(
    "SELECT distinct(b.brand_id), c.subindustry_id, naics_id ",
    "FROM `consumeredgeresearch.apollo_ground_truth.brand` b ",
    "LEFT JOIN `cei-data-science.sandbox.mcc_to_naics` a on b.naics_code = a.naics_id ",
    "LEFT JOIN `consumeredgeresearch.apollo_ground_truth.industry_list` c on b.subindustry_id = c.subindustry_id ",
    "where c.subindustry_id is not null and b.prod_publish_flag = 1 ",
    "order by c.subindustry_id ",
    sep = ""
  )
  rs <- dbSendQuery(con, statement = statement)
  
  data <- NULL
  data <- dbFetch(rs, n = -1)   # extract all rows
  brands <- as.data.frame(data)
  
  #extract the industry codes and naics
  cei_sub_ind <- data.frame(subind = unique(brands[c(2:3)]))
  
  #gather a count/tally of all naics inside each subindustry
  Top_Naics_by_Subind <- data.frame(brands %>% group_by(subindustry_id, naics_id) %>% tally())
  Top_Naics_by_Subind <- na.omit(Top_Naics_by_Subind)
  #discover how many brands CEI has inside of each subindustry
  industry_makeups <- aggregate(n ~ subindustry_id, data = Top_Naics_by_Subind,sum)
  Top_Naics_by_Subind$ind <- industry_makeups$n[match(Top_Naics_by_Subind$subindustry_id,industry_makeups$subindustry_id)]
  Top_Naics_by_Subind$all <- sum(Top_Naics_by_Subind$n)
  
  #discover waighting of the specific naics inside that subindustry
  Top_Naics_by_Subind$perc_ind <- round(Top_Naics_by_Subind$n/Top_Naics_by_Subind$ind,4)*100
  #discover waighting of the specific naics inside all brands - mostly just curious
  Top_Naics_by_Subind$perc_all <- round(Top_Naics_by_Subind$n/Top_Naics_by_Subind$all,4)*100
  
  #Select the top 5 most common NAICS for each subindustry 
  #Order these NAICS by their weight within each subindustry, and then by their overall weight in the BU as a tie breaker
  Top_5 <- data.frame(Top_Naics_by_Subind %>% group_by(subindustry_id) %>% top_n(5,perc_ind)) %>% arrange(desc(perc_ind),desc(perc_all))
  Top_5
}
#Subindustry_descriptions from cei
cei_subindustry_descriptions <- function(email){
  bq_auth(email = email, use_oob = TRUE)
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "consumeredgeresearch",
    dataset = "store_location.OC_algo_QA",
    billing = "consumeredgeresearch"
  )
  statement <- paste(
    "SELECT distinct(c.subindustry_id), c.subindustry_name ",
    "FROM `consumeredgeresearch.apollo_ground_truth.brand` b ",
    "LEFT JOIN `cei-data-science.sandbox.mcc_to_naics` a on b.naics_code = a.naics_id ",
    "LEFT JOIN `consumeredgeresearch.apollo_ground_truth.industry_list` c on b.subindustry_id = c.subindustry_id ",
    "where c.subindustry_id is not null and b.prod_publish_flag = 1 ",
    "order by c.subindustry_id ",
    sep = ""
  )
  rs <- dbSendQuery(con, statement = statement)
  
  # we now fetch records from the resultSet into a data.frame
  data <- NULL
  data <- dbFetch(rs, n = -1)   # extract all rows
  subindust_names <- as.data.frame(data)
  subindust_names
}
#Brand Entries into CEI Internals
Brand_Entry <- function(initial_search){
  for (i in 1:nrow(initial_search)){
    remDr$navigate("https://internals.consumer-edge.com/brand/add")
    Sys.sleep(.5)
    brand_name <- remDr$findElement('id','brand_name')
    brand_name$sendKeysToElement(list(initial_search$brand[i]))
    Sys.sleep(.25)
    brand_url <- remDr$findElement('id','brand_url_addr')
    brand_url$sendKeysToElement(list(initial_search$brand_url_addr[i]))
    Sys.sleep(.25)
    industry_option <- remDr$findElement('id','industry')
    industry_option$clickElement()
    #UNFORTUNATELY - *cough cough* - the industry codes on the site are not sequential... and must be manually checked...
    if(initial_search$subind_id[i] == 1000){remDr$findElement('xpath','//*[@id="industry"]/option[1]')$clickElement()}
    if(initial_search$subind_id[i] == 1001){remDr$findElement('xpath','//*[@id="industry"]/option[2]')$clickElement()}
    if(initial_search$subind_id[i] == 1002){remDr$findElement('xpath','//*[@id="industry"]/option[3]')$clickElement()}
    if(initial_search$subind_id[i] == 1003){remDr$findElement('xpath','//*[@id="industry"]/option[4]')$clickElement()}
    if(initial_search$subind_id[i] == 1004){remDr$findElement('xpath','//*[@id="industry"]/option[5]')$clickElement()}
    if(initial_search$subind_id[i] == 1005){remDr$findElement('xpath','//*[@id="industry"]/option[6]')$clickElement()}
    if(initial_search$subind_id[i] == 1006){remDr$findElement('xpath','//*[@id="industry"]/option[7]')$clickElement()}
    if(initial_search$subind_id[i] == 1007){remDr$findElement('xpath','//*[@id="industry"]/option[8]')$clickElement()}
    if(initial_search$subind_id[i] == 1008){remDr$findElement('xpath','//*[@id="industry"]/option[9]')$clickElement()}
    if(initial_search$subind_id[i] == 1009){remDr$findElement('xpath','//*[@id="industry"]/option[10]')$clickElement()}
    if(initial_search$subind_id[i] == 1010){remDr$findElement('xpath','//*[@id="industry"]/option[12]')$clickElement()}
    if(initial_search$subind_id[i] == 1011){remDr$findElement('xpath','//*[@id="industry"]/option[13]')$clickElement()}
    if(initial_search$subind_id[i] == 1012){remDr$findElement('xpath','//*[@id="industry"]/option[14]')$clickElement()}
    if(initial_search$subind_id[i] == 1013){remDr$findElement('xpath','//*[@id="industry"]/option[15]')$clickElement()}
    if(initial_search$subind_id[i] == 1014){remDr$findElement('xpath','//*[@id="industry"]/option[16]')$clickElement()}
    if(initial_search$subind_id[i] == 1015){remDr$findElement('xpath','//*[@id="industry"]/option[17]')$clickElement()}
    if(initial_search$subind_id[i] == 1016){remDr$findElement('xpath','//*[@id="industry"]/option[18]')$clickElement()}
    if(initial_search$subind_id[i] == 1017){remDr$findElement('xpath','//*[@id="industry"]/option[19]')$clickElement()}
    if(initial_search$subind_id[i] == 1018){remDr$findElement('xpath','//*[@id="industry"]/option[20]')$clickElement()}
    if(initial_search$subind_id[i] == 1019){remDr$findElement('xpath','//*[@id="industry"]/option[21]')$clickElement()}
    if(initial_search$subind_id[i] == 1020){remDr$findElement('xpath','//*[@id="industry"]/option[24]')$clickElement()}
    if(initial_search$subind_id[i] == 1021){remDr$findElement('xpath','//*[@id="industry"]/option[25]')$clickElement()}
    if(initial_search$subind_id[i] == 1022){remDr$findElement('xpath','//*[@id="industry"]/option[26]')$clickElement()}
    if(initial_search$subind_id[i] == 1023){remDr$findElement('xpath','//*[@id="industry"]/option[27]')$clickElement()}
    if(initial_search$subind_id[i] == 1024){remDr$findElement('xpath','//*[@id="industry"]/option[22]')$clickElement()}
    if(initial_search$subind_id[i] == 1025){remDr$findElement('xpath','//*[@id="industry"]/option[23]')$clickElement()}
    if(initial_search$subind_id[i] == 1026){remDr$findElement('xpath','//*[@id="industry"]/option[28]')$clickElement()}
    if(initial_search$subind_id[i] == 1027){remDr$findElement('xpath','//*[@id="industry"]/option[29]')$clickElement()}
    if(initial_search$subind_id[i] == 1028){remDr$findElement('xpath','//*[@id="industry"]/option[30]')$clickElement()}
    if(initial_search$subind_id[i] == 1029){remDr$findElement('xpath','//*[@id="industry"]/option[31]')$clickElement()}
    if(initial_search$subind_id[i] == 1030){remDr$findElement('xpath','//*[@id="industry"]/option[32]')$clickElement()}
    if(initial_search$subind_id[i] == 1031){remDr$findElement('xpath','//*[@id="industry"]/option[33]')$clickElement()}
    if(initial_search$subind_id[i] == 1032){remDr$findElement('xpath','//*[@id="industry"]/option[34]')$clickElement()}
    if(initial_search$subind_id[i] == 1033){remDr$findElement('xpath','//*[@id="industry"]/option[35]')$clickElement()}
    if(initial_search$subind_id[i] == 1034){remDr$findElement('xpath','//*[@id="industry"]/option[36]')$clickElement()}
    if(initial_search$subind_id[i] == 1035){remDr$findElement('xpath','//*[@id="industry"]/option[37]')$clickElement()}
    if(initial_search$subind_id[i] == 1036){remDr$findElement('xpath','//*[@id="industry"]/option[38]')$clickElement()}
    if(initial_search$subind_id[i] == 1037){remDr$findElement('xpath','//*[@id="industry"]/option[40]')$clickElement()}
    if(initial_search$subind_id[i] == 1038){remDr$findElement('xpath','//*[@id="industry"]/option[41]')$clickElement()}
    if(initial_search$subind_id[i] == 1039){remDr$findElement('xpath','//*[@id="industry"]/option[42]')$clickElement()}
    if(initial_search$subind_id[i] == 1040){remDr$findElement('xpath','//*[@id="industry"]/option[43]')$clickElement()}
    if(initial_search$subind_id[i] == 1041){remDr$findElement('xpath','//*[@id="industry"]/option[44]')$clickElement()}
    if(initial_search$subind_id[i] == 1042){remDr$findElement('xpath','//*[@id="industry"]/option[45]')$clickElement()}
    if(initial_search$subind_id[i] == 1043){remDr$findElement('xpath','//*[@id="industry"]/option[46]')$clickElement()}
    if(initial_search$subind_id[i] == 1044){remDr$findElement('xpath','//*[@id="industry"]/option[47]')$clickElement()}
    if(initial_search$subind_id[i] == 1045){remDr$findElement('xpath','//*[@id="industry"]/option[48]')$clickElement()}
    if(initial_search$subind_id[i] == 1046){remDr$findElement('xpath','//*[@id="industry"]/option[49]')$clickElement()}
    if(initial_search$subind_id[i] == 1047){remDr$findElement('xpath','//*[@id="industry"]/option[39]')$clickElement()}
    Sys.sleep(.25)
    naics_option <- remDr$findElement('id','naics_code')
    naics_option$sendKeysToElement(list(as.character(initial_search$naics[i])))
    Sys.sleep(.25)
    #dev switched on#
    remDr$findElement('xpath','//*[@id="dev_publish_flag"]/option[2]')$clickElement()
    Sys.sleep(.25)
    #submit
    remDr$findElement(using = "id", value = "submit")$clickElement()
    Sys.sleep(.25)
    #accept alert
    remDr$acceptAlert()
  }
}
#Clean commonly formatted urls for CEI
url_cleanse <- function(url){
  paste("www.",gsub("www\\.","",gsub("http://","",gsub("https://","",gsub("https://www\\.","",tolower(url) )))),sep="")
}
#Initial Rule entry to ensure time saved on Day 2 of review
character_length_ruling <- function(brand_names){
  rule_names <- data.frame(rules = gsub("(\\.)*COM","",gsub("  "," ",gsub("(\\.|\\&|\\+|\\!|\\')","",gsub("","", brand_names)))))
  rule_parameters <- rule_names %>% separate(rules,c("Word_One","Word_Two","Word_Three","Word_Four","Word_Five"),sep=" ") %>% replace(is.na(.),"") %>%
    mutate(Word_One_Length = nchar(Word_One), Word_Two_Length = nchar(Word_Two), Word_Three_Length = nchar(Word_Three),
           Word_Four_Length = nchar(Word_Four), Word_Five_length = nchar(Word_Five)) %>% replace(is.na(.),0)
  
  #WE must take extreme care and caution given the length of the brand name and the number of spaces within
  #By removing space and other special characters (unlikely to appear in DB) we can get a true(ish?) idea for their length
  rule_name_lengths <- as.numeric(nchar(gsub(" ","",rule_names$rules)))
  
  rule_selection <- data.frame(rule_names,rule_name_lengths, rule_parameters) %>% mutate(rule_name_lengths = as.numeric(rule_name_lengths), surname_check = NA)

  for(i in 1:nrow(rule_selection)){
    for(j in 1:nrow(surnames)){
      if((rule_selection$surname_check[i] == F | is.na(rule_selection$surname_check[i]) == T)){
        rule_selection$surname_check[i] <- grepl(paste(surnames$Name[j],'$',sep=""),rule_selection$rules[i])
      }
    }
  }

  #Understanding the lengths (the number of characters) allows me to cut off endings, if brand name is even long enough.
  # I WANT this to be shy and miss things. This is a conservative first step to aggregate together, on top of Kevin's algo
  # the proper/ideal mids, phone numbers, mcc's, etc. that will allow for rule sharpening down the line.
  
  for (i in 1:nrow(rule_selection)){
    if((rule_selection$Word_One_Length[i]<=5) & (rule_selection$Word_Two_Length[i] <= 5) & (rule_selection$rule_name_lengths[i] <= 9)){rule_selection$rules[i] <- left(rule_selection$rules[i],(nchar(rule_selection$rules[i])))}
    if((rule_selection$rule_name_lengths[i]<=13) & (rule_selection$rule_name_lengths[i] > 9) & (rule_selection$Word_One_Length[i]>5) & (rule_selection$surname_check[i] == F)){rule_selection$rules[i] <- left(rule_selection$rules[i],(nchar(rule_selection$rules[i])-2))}
    if( ((rule_selection$rule_name_lengths[i]<=17) & (rule_selection$rule_name_lengths[i] > 13)) & (rule_selection$Word_One_Length[i]>5) & (rule_selection$surname_check[i] == F)){rule_selection$rules[i] <- left(rule_selection$rules[i],(nchar(rule_selection$rules[i])-3))}
    if((rule_selection$rule_name_lengths[i] > 17) & (rule_selection$surname_check[i] == F)){rule_selection$rules[i] <- left(rule_selection$rules[i],(nchar(rule_selection$rules[i])-4))}
  }
  
  for (i in 1:nrow(rule_selection)){
    if(nchar(rule_selection$rules[i]) == nchar(rule_names$rules[i]) & (rule_selection$Word_Three_Length[i]>=4) & (rule_selection$surname_check[i] == F)){
      rule_selection$rules[i] <- left(rule_selection$rules[i],(nchar(rule_selection$rules[i])-2))
    }
    
      }
  
  rule_selection$rules <- ifelse(rule_selection$Word_One_Length <=12,
                                 paste("(^|\\s|\\*\\s*)",str_replace_all(rule_selection$rules," ","\\\\s*"),".*",sep=""),
                                 paste("%",gsub(" ","%",rule_selection$rules),"%",sep=""))
  rule_selection <- rule_selection %>% mutate(rule_type = ifelse(Word_One_Length<=12,"Regex","Like")) %>% select(rules,rule_type)
  #Reassign brand names and info to the rules themselves to ensure human eyes can review if needed
  rule_selection <- cbind(rule_selection,initial_search$brand_links)  
  rule_selection <- cbind(rule_selection,unlist(initial_search$brand_id))
  rule_selection <- cbind(rule_selection,initial_search$brand)
  #We cannot enter NA rules, we will piss everyone off, so ensure just one more time there are no NA's lurking
  rule_selection <- na.omit(rule_selection)
  
  remDr = CEI_Login("{your_ip}", "cpachulski@consumer-edge.com","{your_password}")
  i = 1
  for (i in 1:nrow(rule_selection)){
  remDr$navigate(rule_selection$`initial_search$brand_links`[i])
  Sys.sleep(3)
  remDr$findElement("id",'brand-ats-btn')$clickElement()
  Sys.sleep(1)
  remDr$findElement('xpath','//*[@id="brand-ats-btn"]/a/button')$clickElement()
  Sys.sleep(1)
  remDr$findElement('xpath','//*[@id="map_unmap"]/option[2]')$clickElement()
  if(rule_selection$rule_type[i] == "Regex"){remDr$findElement('xpath','//*[@id="match_method"]/option[2]')$clickElement()}else{remDr$findElement('xpath','//*[@id="match_method"]/option[3]')$clickElement()}
  Sys.sleep(1)
  Merch_Desc <- remDr$findElement('id','merchdesc')
  Merch_Desc$sendKeysToElement(list(rule_selection$rules[i]))
  Sys.sleep(1)
  remDr$findElement(using = "id", value = "submit")$clickElement()
  Sys.sleep(.25)
  #accept alert
  remDr$acceptAlert()
  }}
#Raw List of Brand Scorecard_info
og_list_retrieval <- function(google_doc, google_sheet){
  drive_auth(email = "cpachulski@cei.ventures",use_oob=TRUE)
  gs4_auth(email = "cpachulski@cei.ventures",use_oob=TRUE)
  
  og_list <- range_read(drive_get(google_doc),google_sheet)
  og_list
}
#Everyday Query Pull - See the results currently tagged regardless of day
current_brand_tagging <- function(google_doc, google_sheet){
  drive_auth(email = "cpachulski@cei.ventures",use_oob=TRUE)
  gs4_auth(email = "cpachulski@cei.ventures",use_oob=TRUE)
  
  Day_2 <- range_read(drive_get(google_doc),google_sheet)
  og_list <- Day_2
  
  Brand_List <- list()
  
  for (i in 1:length(Day_2$brand)){
    Brands <- paste('"',Day_2$brand[i], '"', sep = "")
    Brand_List <- paste(Brand_List,Brands, sep = ", ")
  }
  
  Brand_List <- gsub("^\\,\\s+","", Brand_List)

  bq_auth(email = "cpachulski@cei.ventures", use_oob = TRUE)
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "consumeredgeresearch",
    dataset = "store_location.OC_algo_QA",
    billing = "consumeredgeresearch"
  )
  
  List_Of_Queries <- list()
  options(scipen = 20)
  statement <- paste(
    "SELECT DISTINCT m.mid,m.merchdesc,m.mcc, b.brand_name, b.subindustry_id, m.spend_amount,m.last_trans_date ",
    "FROM `consumeredgeresearch.apollo_us_ats_ml.mid_enhanced` m ",
    "left join `consumeredgeresearch.apollo_panel_map.mcc_category` mc on mc.mcc = m.mcc ",
    "left join `consumeredgeresearch.apollo_us_ats_ml.mid_match_brand_ats` mm on mm.merchid = m.merchid ",
    "left join `consumeredgeresearch.apollo_ground_truth.brand` b on b.brand_id = mm.main_brand_id ",
    "left join `consumeredgeresearch.apollo_ats_ml.mid_match_ats` mb on mb.main_brand_id = b.brand_id ",
    "left join `cei-data-science.dev.mid_match` kmm on kmm.merchid = m.merchid ",
    "left join `consumeredgeresearch.apollo_ground_truth.brand` b4 on b4.brand_id = kmm.main_brand_id ",
    'Where b.brand_name in (',Brand_List,') ',
    'Order By spend_amount desc',
    sep = ""
  )
  Query_Results <- dbSendQuery(con, statement = statement) %>% dbFetch(rs, n = -1) %>% as.data.frame()
  
  for(i in 1:length(unique(og_list$brand))){
    List_Of_Queries[[i]] <- Query_Results[which(Query_Results$brand_name == unique(og_list$brand)[i]),]
  }
  List_Of_Queries
}
#MCC Review
mcc_review <- function(List_Of_Queries){
  bq_auth(email = "cpachulski@cei.ventures", use_oob = TRUE)
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "consumeredgeresearch",
    dataset = "store_location.OC_algo_QA",
    billing = "consumeredgeresearch"
  )
  
  
  statement <- paste(
    "SELECT distinct(b.subindustry_id),m.mcc, count(m.mcc) as mcc_count ",
    "FROM `consumeredgeresearch.apollo_us_ats_ml.mid_enhanced` m ",
    "left join `consumeredgeresearch.apollo_panel_map.mcc_category` mc on mc.mcc = m.mcc ",
    "left join `consumeredgeresearch.apollo_us_ats_ml.mid_match_brand_ats` mm on mm.merchid = m.merchid ",
    "left join `consumeredgeresearch.apollo_ground_truth.brand` b on b.brand_id = mm.main_brand_id ",
    "where b.brand_id is not null and b.prod_publish_flag = 1 ",
    "group by 1,2 ",
    "order by subindustry_id, mcc_count desc, m.mcc desc ",
    sep = ""
  )
  mcc_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(rs, n = -1) %>% as.data.frame()
  
  #View(mcc_tbl)
  mcc_makeups <- aggregate(mcc_count ~ subindustry_id, mcc_tbl,sum)
  mcc_tbl$subind_totals <- mcc_makeups$mcc_count[match(mcc_tbl$subindustry_id,mcc_makeups$subindustry_id)]
  mcc_tbl$Perc_makeup <- round(mcc_tbl$mcc_count/mcc_tbl$subind_totals,4)*100
  mcc_tbl <- mcc_tbl[which(mcc_tbl$Perc_makeup >= .10),]
  
  mcc_unmappings <- NULL
  mcc_unmapping <- NULL
  mcc_overview <- NULL
  List_Of_Queries <- Filter(function(x) dim(x)[1], List_Of_Queries)

  for (i in 1:length(List_Of_Queries)){
    # unique(List_Of_Queries[[i]]$subindustry_id)
    actual_mccs <- unique(List_Of_Queries[[i]]$mcc)
    actual_mccs <- actual_mccs[which(nchar(actual_mccs) >= 4)]
    if(identical(actual_mccs,character(0))){actual_mccs <- "0000"}
    expected <- unique(mcc_tbl[which(mcc_tbl$subindustry_id == unique(List_Of_Queries[[i]]$subindustry_id)),])
    expected_mccs <- expected$mcc
    mccs_review <- NULL
    for (j in 1:length(actual_mccs)){
      mccs <- NULL
      mcc_present <- if(identical(grep(actual_mccs[j],expected_mccs),integer(0))){NA}else{grep(actual_mccs[j],expected_mccs)}
      mccs <- rbind(mccs,mcc_present)
      mccs <- cbind(mccs, actual_mccs[j])
      mccs_review <- as.data.frame(rbind(mccs_review,mccs))
    }
    mccs_review$perc <- expected$Perc_makeup[match(mccs_review$V2,expected$mcc)]
    mccs_review <- mccs_review[-1]
    mccs_review <- mccs_review %>% arrange(desc(perc))
    if(sum(is.na(mccs_review$perc)) == 0){mcc_checks <- data.frame(brand = unique(List_Of_Queries[[i]]$brand_name), V2 = NA)}else{
      mcc_checks <- data.frame(brand = unique(List_Of_Queries[[i]]$brand_name), bad_mccs <- mccs_review %>% filter(is.na(perc)) %>% select(V2))
    }
    actual_mcc_spend <- List_Of_Queries[[i]] %>% group_by(mcc) %>% summarise(mcc_spend = sum(spend_amount)) %>% arrange(desc(mcc_spend)) %>% ungroup()
    actual_mcc_spend$perc <- round(actual_mcc_spend$mcc_spend/sum(actual_mcc_spend$mcc_spend),4)*100
    #tryCatch(expr = {mcc_checks$spend_perc <- actual_mcc_spend$perc[match(mcc_checks$bad_mccs....NA,actual_mcc_spend$mcc)]}, error = function(e){mcc_checks$spend_perc <- actual_mcc_spend$perc[match(mcc_checks$V2,actual_mcc_spend$mcc)]})
    if(identical(actual_mcc_spend$perc[match(mcc_checks$bad_mccs....NA,actual_mcc_spend$mcc)],numeric(0))==T){mcc_checks$spend_perc <- actual_mcc_spend$perc[match(mcc_checks$V2,actual_mcc_spend$mcc)]}else(mcc_checks$spend_perc <- actual_mcc_spend$perc[match(mcc_checks$bad_mccs....NA,actual_mcc_spend$mcc)])
    status_log <- NULL
    #j = 1
    for (j in 1:nrow(mcc_checks)){
      if(identical(grep("TRUE",str_detect(left(mcc_checks$V2[j],2), left(unique(expected_mccs),2))),integer(0))){status = "remove"}else{status = "keep"}
      status_log <- rbind(status_log,status)
    }
    mcc_checks <- cbind(mcc_checks,status_log)
    mcc_checks <- mcc_checks[which(mcc_checks$status_log != "keep"),]
    mcc_unmappings <- rbind(mcc_checks, mcc_unmappings)
    mcc_unmappings <- na.omit(mcc_unmappings)
    List_Of_Queries[[i]]$mcc_rec_rem <- mcc_unmappings$status_log[match(List_Of_Queries[[i]]$mcc,mcc_unmappings$V2)]
    List_Of_Queries[[i]]$mcc_rec_rem[is.na(List_Of_Queries[[i]]$mcc_rec_rem)] <- "keep"
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse(grepl("^8(6|9|3).*",List_Of_Queries[[i]]$mcc)==T,"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse(grepl("7399",List_Of_Queries[[i]]$mcc)==T,"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse(grepl("5734",List_Of_Queries[[i]]$mcc)==T,"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse(grepl("7299",List_Of_Queries[[i]]$mcc)==T,"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse(grepl("5(1|3)99",List_Of_Queries[[i]]$mcc)==T,"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse(grepl("4816",List_Of_Queries[[i]]$mcc)==T,"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("0742",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1014" | List_Of_Queries[[i]]$subindustry_id == "1017" |List_Of_Queries[[i]]$subindustry_id == "1018" | List_Of_Queries[[i]]$subindustry_id == "1019"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("7298",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1020" | List_Of_Queries[[i]]$subindustry_id == "1016" | List_Of_Queries[[i]]$subindustry_id == "1011"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5499",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1020" | List_Of_Queries[[i]]$subindustry_id == "1016" | List_Of_Queries[[i]]$subindustry_id == "1011"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("7230",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1011" | List_Of_Queries[[i]]$subindustry_id == "1016"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5499",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1022"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("8099",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1011"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("7372",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1029"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5734",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1029"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5732",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1013"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("7922",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1046"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5699",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1000" | List_Of_Queries[[i]]$subindustry_id == "1001" | List_Of_Queries[[i]]$subindustry_id == "1002"  | List_Of_Queries[[i]]$subindustry_id == "1003"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5691",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1000" | List_Of_Queries[[i]]$subindustry_id == "1001" | List_Of_Queries[[i]]$subindustry_id == "1002"  | List_Of_Queries[[i]]$subindustry_id == "1003"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("6051",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1030"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("573\\d$",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1019" | List_Of_Queries[[i]]$subindustry_id == "1018"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("56\\d\\d",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1000" | List_Of_Queries[[i]]$subindustry_id == "1001" |List_Of_Queries[[i]]$subindustry_id == "1002" | List_Of_Queries[[i]]$subindustry_id == "1003"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("566\\d$",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1014"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5099",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1014"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5094",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1000" | List_Of_Queries[[i]]$subindustry_id == "1001" |List_Of_Queries[[i]]$subindustry_id == "1002" | List_Of_Queries[[i]]$subindustry_id == "1003"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("504\\d",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1000" | List_Of_Queries[[i]]$subindustry_id == "1001" |List_Of_Queries[[i]]$subindustry_id == "1002" | List_Of_Queries[[i]]$subindustry_id == "1003"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5131",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1000" | List_Of_Queries[[i]]$subindustry_id == "1001" |List_Of_Queries[[i]]$subindustry_id == "1002" | List_Of_Queries[[i]]$subindustry_id == "1003" | List_Of_Queries[[i]]$subindustry_id == "1018" | List_Of_Queries[[i]]$subindustry_id == "1019"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5691",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1040" | List_Of_Queries[[i]]$subindustry_id == "1044"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5712",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1014" | List_Of_Queries[[i]]$subindustry_id == "1017"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5046",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1014" | List_Of_Queries[[i]]$subindustry_id == "1017"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("7221",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1013"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5699",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1020"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5631",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1016"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5712",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1004"  | List_Of_Queries[[i]]$subindustry_id == "1005"  | List_Of_Queries[[i]]$subindustry_id == "1006"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("7392",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1029"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("7333",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1026"  | List_Of_Queries[[i]]$subindustry_id == "1046"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("5261",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1014"  | List_Of_Queries[[i]]$subindustry_id == "1017"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("2741",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1008"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    List_Of_Queries[[i]]$mcc_rec_rem <- ifelse((grepl("8043",List_Of_Queries[[i]]$mcc)==T) & (List_Of_Queries[[i]]$subindustry_id == "1031"),"keep",List_Of_Queries[[i]]$mcc_rec_rem)
    
    
    mcc_checks <- List_Of_Queries[[i]][which(List_Of_Queries[[i]]$mcc_rec_rem != "keep"),] %>% select(brand_name,mcc,mcc_rec_rem) %>% distinct()
    mcc_unmapping <- rbind(mcc_checks, mcc_unmapping)
    mcc_unmapping <- na.omit(mcc_unmapping)
    
    mcc_errors <- round(nrow(List_Of_Queries[[i]][which(List_Of_Queries[[i]]$mcc_rec_rem == "remove"),])/nrow(List_Of_Queries[[i]]),2)
    mcc_successes <- round(nrow(List_Of_Queries[[i]][which(List_Of_Queries[[i]]$mcc_rec_rem == "keep"),])/nrow(List_Of_Queries[[i]]),2)
    if(mcc_errors >= .50){List_Of_Queries[[i]]$mcc_rec_rem <- "scorecard_review"}
    if(mcc_successes >= .90){List_Of_Queries[[i]]$mcc_rec_rem <- "keep"}
    mcc_overview_component <- cbind(brand = unique(List_Of_Queries[[i]]$brand_name), as.data.frame(t(summary(as.factor(List_Of_Queries[[i]]$mcc_rec_rem)))) )
    if((colnames(mcc_overview_component)[2]=="keep") & (ncol(mcc_overview_component)<3) ){mcc_overview_component <- data.frame(mcc_overview_component,remove = NA,scorecard_review = NA)}
    if(colnames(mcc_overview_component)[2]=="scorecard_review"){mcc_overview_component <- data.frame(brand = mcc_overview_component[1],keep = NA, remove = NA,scorecard_review = mcc_overview_component[2])}
    if(ncol(mcc_overview_component)==3){mcc_overview_component <- data.frame(mcc_overview_component,scorecard_review = NA)}
    # mcc_overview_component[is.na(mcc_overview_component)] <- 0
    # mcc_overview <- rbind(mcc_overview, mcc_overview_component)
    # mcc_overview$query_value <- seq(nrow(mcc_overview))
  }
  
  #View(mcc_overview)
  mcc_overview$query_value <- seq(nrow(mcc_overview))
  List_Of_Queries
}
#Top 5 Mids by aggregated spend
mid_code_reviews <- function(List_Of_Queries){
  top_mids <- NULL
  for (i in 1:length(List_Of_Queries)){
    List_Of_Queries[[i]]$mid <- gsub("^0+","",List_Of_Queries[[i]]$mid)
    List_Of_Queries[[i]] <- List_Of_Queries[[i]][!grepl("^174.*$", List_Of_Queries[[i]]$mid),]
    List_Of_Queries[[i]] <- List_Of_Queries[[i]][which(nchar(List_Of_Queries[[i]]$mid)>0),]
    #List_Of_Queries[[i]] <- List_Of_Queries[[i]][!grepl("", List_Of_Queries[[i]]$mid),]
    List_Of_Queries[[i]] <- na.omit(List_Of_Queries[[i]])
    ind_top_mid <- aggregate(spend_amount ~ mid,data = List_Of_Queries[[i]],sum) %>% arrange(desc(spend_amount)) %>% slice(1:5) %>% mutate(brand_name = unique(List_Of_Queries[[i]]$brand_name))%>% mutate(map_dec = NA)
    
    for(j in 1:nrow(ind_top_mid)){
      options(scipen = 20)
      statement <- paste(
        "SELECT DISTINCT m.mid,m.merchdesc,m.mcc, b.brand_name, m.spend_amount, ",
        "FROM `consumeredgeresearch.apollo_us_ats_ml.mid_enhanced` m ",
        "left join `consumeredgeresearch.apollo_panel_map.mcc_category` mc on mc.mcc = m.mcc ",
        "left join `consumeredgeresearch.apollo_us_ats_ml.mid_match_brand_ats` mm on mm.merchid = m.merchid ",
        "left join `consumeredgeresearch.apollo_ground_truth.brand` b on b.brand_id = mm.main_brand_id ",
        'Where m.mid like "%',ind_top_mid$mid[j],'%" ',
        'Order By spend_amount desc ',
        'Limit 2000 ',
        sep = ""
      )
      rs <- dbSendQuery(con, statement = statement)
      
      # we now fetch records from the resultSet into a data.frame
      mid_data <- NULL
      mid_data <- dbFetch(rs, n = -1)   # extract all rows
      mid_results <- as.data.frame(mid_data)
      check <- is.na(mid_results$brand_name[is.na(mid_results$brand_name)])
      if(identical(check,logical(0))==T){check <- F}
      if((length(unique(mid_results$brand_name))==2) & (unique(check) == T)){ind_top_mid$map_dec[j] = "map"}else{ind_top_mid$map_dec[j] = "leave"}
      ind_top_mid$rule_entry[j] <- paste("^0*",ind_top_mid$mid[j],"|",sep="")
      ind_top_mid$spend_amount[j] <- if(identical(mid_results %>% filter(is.na(brand_name)) %>% group_by(mid) %>% summarise(spend_amount = sum(spend_amount)) %>% ungroup() %>% select(spend_amount) %>% unlist(),numeric(0))){0}else{mid_results %>% filter(is.na(brand_name)) %>% group_by(mid) %>% summarise(spend_amount = sum(spend_amount)) %>% ungroup() %>% select(spend_amount) %>% unlist()}
    }
    
    ind_top_mid <- ind_top_mid[which(ind_top_mid$map_dec == "map"),]
    if(nrow(ind_top_mid)==0){ind_top_mid = data.frame(brand_name = unique(List_Of_Queries[[i]]$brand_name), rule = NA, spend = NA )}
    rule_entry <- data.frame(brand_name = unique(ind_top_mid$brand_name), rule = gsub("\\|$","",paste(unlist(ind_top_mid$rule_entry),collapse = "",sep="")), spend = round(sum(ind_top_mid$spend_amount),2))
    top_mids <- rbind(top_mids, rule_entry)
  }
  refined_top_mids <- top_mids[which(top_mids$spend > 0.00),]
  
  refined_top_mids$brand_urls <- og_list$brand_links[match(refined_top_mids$brand_name,og_list$brand)]
  rownames(refined_top_mids) <- seq(nrow(refined_top_mids)) 
  refined_top_mids <- refined_top_mids %>% arrange(desc(spend))
  refined_top_mids
}
#Channel Assignment List of Query Results
subchannel_review <- function(google_doc, google_sheet){
  drive_auth(email = "cpachulski@cei.ventures",use_oob=TRUE)
  gs4_auth(email = "cpachulski@cei.ventures",use_oob=TRUE)
  
  Day_4 <- range_read(drive_get(google_doc),google_sheet)
  # 
  # data$brand <- gsub("\\.CA","\\.COM",gsub(" US$","",gsub("\\.\\s"," ",gsub(" LLC","",gsub(" CA$","",gsub(" INC\\.","",gsub("\\,","",gsub("-","",gsub("'","",gsub("CO\\.","",gsub("\\s*\\(.*\\)\\s*","",data$brand)))))))))))
  # data <- data[c(1,2)]
  # data <- data[!grepl("\\.ca(\\/*)$",data$brand_url_addr),]
  # data <- data[!grepl("\\/ca$",data$brand_url_addr),]
  # data <- unique(data)
  
  og_list <- Day_4
  
  Brand_List <- list()
  
  for (i in 1:length(Day_4$brand)){
    Brands <- paste('"',Day_4$brand[i], '"', sep = "")
    Brand_List <- paste(Brand_List,Brands, sep = ", ")
  }
  
  Brand_List <- gsub("^\\,\\s+","", Brand_List)
  
  
  Start_Time <- Sys.time()
  library(DBI)
  bq_auth(email = "cpachulski@cei.ventures", use_oob = TRUE)
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "consumeredgeresearch",
    dataset = "store_location.OC_algo_QA",
    billing = "consumeredgeresearch"
  )
  
  List_Of_Queries <- list()
  
  statement <- paste(
    "SELECT DISTINCT b.brand_name, m.merchdesc, m.state, m.top_state_abbr, pct_state_trans, b.brand_id, mb.channel ",
    "FROM `consumeredgeresearch.apollo_us_ats_ml.mid_enhanced` m ",
    "left join `consumeredgeresearch.apollo_panel_map.mcc_category` mc on mc.mcc = m.mcc ",
    "left join `consumeredgeresearch.apollo_us_ats_ml.mid_match_brand_ats` mm on mm.merchid = m.merchid ",
    "left join `consumeredgeresearch.apollo_ground_truth.brand` b on b.brand_id = mm.main_brand_id ",
    "left join `consumeredgeresearch.apollo_ats_ml.mid_match_ats` mb on mb.main_brand_id = b.brand_id ",
    "left join `cei-data-science.dev.mid_match` kmm on kmm.merchid = m.merchid ",
    "left join `consumeredgeresearch.apollo_ground_truth.brand` b4 on b4.brand_id = kmm.main_brand_id ",
    'Where b.brand_name in (',Brand_List,') ',
    sep = ""
  )
  rs <- dbSendQuery(con, statement = statement)
  
  # we now fetch records from the resultSet into a data.frame
  data <- NULL
  data <- dbFetch(rs, n = -1)   # extract all rows
  Query_Results <- as.data.frame(data)
  
  
  
  for(i in 1:length(unique(og_list$brand))){
    List_Of_Queries[[i]] <- Query_Results[which(Query_Results$brand_name == unique(og_list$brand)[i]),]
  }
  List_Of_Queries
}
#Begin####
#connect to google drive and arrange for non-interactive sessions
drive_auth(email = "cpachulski@cei.ventures",use_oob=TRUE)
gs4_auth(email = "cpachulski@cei.ventures",use_oob=TRUE)
surnames <- drive_get("surnames") %>% range_read("name") %>% as.data.frame()

#connect to brand list - this list changes every other day unfortunately...
ss <- drive_get("Affiliated_Brands")
initial_search <- as.data.frame(range_read(ss,"LS")) %>% filter(initials == "CP") %>% filter(`Pre-Existing` == 0)#%>% filter(is.na(`CEI brand match`)==T)

#rename columns for consistency through script
colnames(initial_search)[1] <- "brand"
colnames(initial_search)[2] <- "brand_url_addr"

#Clean brands from unwanted characters for SEO & duplicate removal 
initial_search <- initial_search %>% mutate(brand = gsub("\\.CA","\\.COM",gsub(" US$","",gsub("\\.\\s"," ",gsub(" LLC","",gsub(" CA$","",gsub(" INC\\.","",gsub("\\,","",gsub("-","",gsub("'","",gsub("CO\\.","",gsub("\\s*\\(.*\\)\\s*","",brand)))))))))))) %>%
                  select(brand, brand_url_addr) %>% filter(!grepl("\\.ca(\\/*)$", brand_url_addr)) %>% filter(!grepl("\\/ca$", brand_url_addr)) %>% filter(!grepl("\\/\\/ca", brand_url_addr)) %>% distinct() %>% `rownames<-`(seq(nrow(.))) %>%
                  mutate(naics = NA) %>% mutate(info = NA)

number_of_brands <- nrow(initial_search)
number_of_brands
#glimspe - commented out - to ensure on new lists our cleanse has occured properly for human eyes
#glimpse(initial_search)


#Brand Scorecard Information Retreival####
#Ping docker instance to begin chromedriver via chrome and initial_search functions
remDr = chrome("{your_ip}")
for(i in 1:nrow(initial_search)){
  initial_search$naics[i] <- tryCatch(expr = {suppressMessages(initial_google(initial_search))}, error = function(e){})
}
#With raw naics gathered, apply naic-naic overrides for manufacturing and wholesale
initial_search$naics <- naic_to_naic(initial_search$naics)

#Allow Brand Keywords to override raw naics findings if criteria are met
initial_search$naics <- brand_to_naic_overrides(initial_search$naics)

# Fill in remaining 'empties' with 'All Other Retail'
initial_search$naics[is.na(initial_search$naics)] <- "453998"

#Connect with CEI BQ tables
cei_naics <- cei_naics_bq_call("cpachulski@cei.ventures")
Top_5 <- cei_subind_IDs_bq_call("cpachulski@cei.ventures")
subindust_names <- cei_subindustry_descriptions("cpachulski@cei.ventures")

initial_search <- initial_search %>%
                  mutate(info = cei_naics$naics_description[match(naics,cei_naics$naics_id)]) %>%
                  mutate(subind_id = Top_5$subindustry_id[match(naics,Top_5$naics_id)]) %>%
                  mutate(subind_id = subind_to_naic_overrides(subind_id)) %>%
                  mutate(subindustry_name = subindust_names$subindustry_name[match(subind_id,subindust_names$subindustry_id)]) #%>% View()
ss <- drive_get("BiP")
sheet_write(initial_search, ss, "Scorecard_info")
initial_search <- range_read(ss, "Scorecard_info")
#initial_search$naics[is.na(initial_search$naics)] <- "454111"
#Implement brands (after review) into CEI site####

remDr = CEI_Login("{your_ip}", "cpachulski@consumer-edge.com","{your_password}")
initial_search <- na.omit(initial_search)
#Prepare the urls for CEI desired formatting (www.)
initial_search$brand_url_addr <- url_cleanse(initial_search$brand_url_addr)
#Auto enter brands and their scorecard info into CEI Internals
Brand_Entry(initial_search)


#After entry, retrieve brand_ids from the site
# This could be done from BQ end, but since we're here, let's jsut use the tool we're already using
remDr$navigate("https://internals.consumer-edge.com/brand/list")
Sys.sleep(65)
remDr$findElement('xpath','//*[@id="brand-table"]/thead/tr/td[1]')$clickElement()
remDr$findElement('xpath','//*[@id="brand-table_length"]/label/select/option[4]')$clickElement()
brand_table <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_node("table") %>%html_table()
initial_search$brand_id <- brand_table$X1[match(initial_search$brand,brand_table$X2)]
for(i in 2:5){
remDr$findElement('xpath', paste('//*[@id="brand-table_paginate"]/span/a[',i,']',sep=""))$clickElement()
Sys.sleep(.25)
#page 2
brand_table <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_node("table") %>%html_table()
initial_search$brand_id <- ifelse(is.na(initial_search$brand_id)==T, brand_table$X1[match(initial_search$brand,brand_table$X2)],initial_search$brand_id)
initial_search$brand_links <- paste("https://internals.consumer-edge.com/brand/detail/",initial_search$brand_id,sep="")
}
#Re-up google credentials so it doesn't default to my personal DB or my mtg DB
drive_auth(email = "cpachulski@cei.ventures", use_oob=TRUE)
gs4_auth(email = "cpachulski@cei.ventures", use_oob=TRUE)

#Write down Brand scorecard info and their corresponding CEI URLS for initial rule creation#
currenDate <- Sys.Date()
ss <- drive_get("BiP")
sheet_write(initial_search,ss,paste(currenDate,"_Newbies",sep=""))
initial_search <- range_read(ss,paste(floor_date(today(),"week",1),"_Newbies",sep=""))
#Day 1 Rule Instroduction - Pre-Kevin's Model####
#Kevin's model, while I'm unclear how it aggregates, clearly does not go by brand name
#entering default brand name rules can save a lot of time, vast majority of the time
character_length_ruling(initial_search$brand)

#Day 2 MCCs - Unmap False positives####
List_Of_Queries = current_brand_tagging("BiP",paste(floor_date(today(),"week",1),"_Newbies",sep=""))
og_list <- og_list_retrieval("BiP",paste(floor_date(today(),"week",1),"_Newbies",sep=""))
MCC_List_Of_Queries <- mcc_review(List_Of_Queries)


#View(mcc_unmapping)
#Enter Day 2 Rules - MCC unmappings####
remDr = CEI_Login("{your_ip}", "cpachulski@consumer-edge.com","{your_password}")
for (i in 1:length(List_Of_Queries)){
    removal_raw <- List_Of_Queries[[i]][which(List_Of_Queries[[i]]$mcc_rec_rem == "remove"),]
    if(nrow(removal_raw) > 0){
        mcc_removals <- as.data.frame(t(data.frame( mccs = t(t(unique(removal_raw$mcc))))))
        mcc_removals <- cbind(mcc_removals, brand = unique(List_Of_Queries[[i]]$brand_name))
        #mcc_removals$URL <- og_list$brand_links[match(mcc_removals$brand,og_list$brand)]
        remDr$navigate(og_list$brand_links[match(mcc_removals$brand,og_list$brand)])
        Sys.sleep(3)
        remDr$findElement("id",'brand-ats-btn')$clickElement()
        Sys.sleep(1)
        remDr$findElement('xpath','//*[@id="brand-ats-btn"]/a/button')$clickElement()
        Sys.sleep(1)
        remDr$findElement('xpath','//*[@id="map_unmap"]/option[3]')$clickElement()
        remDr$findElement('xpath','//*[@id="match_method"]/option[3]')$clickElement()
        Sys.sleep(1)
        MCC_Desc <- remDr$findElement("xpath",'//*[@id="mcc-selectized"]')
        
        mcc_removals <- mcc_removals[c(-ncol(mcc_removals))]
        for (j in 1:ncol(mcc_removals)){
            MCC_Desc$clickElement()
            MCC_Desc$sendKeysToElement(list(mcc_removals[1,j], key = "enter"))
            Sys.sleep(.5)
        }
        Sys.sleep(1)
        remDr$findElement(using = "id", value = "submit")$clickElement()
        Sys.sleep(.25)
        #accept alert
        remDr$acceptAlert()
    }
    }

#Day 3 Mid Aggregation - Wednesday####
List_Of_Queries = current_brand_tagging("BiP",paste(floor_date(today(),"week",1),"_Newbies",sep=""))
og_list = og_list_retrieval("BiP",paste(floor_date(today(),"week",1),"_Newbies",sep=""))

List_Of_Queries <- Filter(function(x) dim(x)[1], List_Of_Queries)

refined_top_mids <- mid_code_reviews(List_Of_Queries)

remDr = CEI_Login("{your_ip}", "cpachulski","{real_password}")

#Mid Rule Entries
for (i in 1:nrow(refined_top_mids)){
    remDr$navigate(refined_top_mids$brand_urls[i])
    Sys.sleep(3)
    remDr$findElement("id",'brand-ats-btn')$clickElement()
    Sys.sleep(1)
    remDr$findElement('xpath','//*[@id="brand-ats-btn"]/a/button')$clickElement()
    Sys.sleep(1)
    remDr$findElement('xpath','//*[@id="map_unmap"]/option[2]')$clickElement()
    remDr$findElement('xpath','//*[@id="match_method"]/option[2]')$clickElement()
    Sys.sleep(1)
    Merch_Desc <- remDr$findElement('id','mid')
    Merch_Desc$sendKeysToElement(list(refined_top_mids$rule[i]))
    Sys.sleep(1)
    remDr$findElement(using = "id", value = "submit")$clickElement()
    Sys.sleep(.25)
    #accept alert
    remDr$acceptAlert()
}


#Day 4 Channel Assignment - Finishing step####
#Subchannel#
og_list <- og_list_retrieval("BiP",paste(floor_date(today(),"week",1),"_Newbies",sep=""))
List_Of_Queries = subchannel_review("BiP",paste(floor_date(today(),"week",1),"_Newbies",sep=""))
# We have now extracted all query results in a list of dataframes
# ... For you python mad lads.... we have a 'dictionary' of dataframes. There, I pandared to you. Laugh at my puns, damn it.
# Anywho!
#Channel Analysis#
Export_Original_Queries <- List_Of_Queries
#List_Of_Queries <- Export_Original_Queries
#length(Export_Original_Queries)
#length(List_Of_Queries)
#Line above very important to retain Query results so we don't have to re-query and cost ourselves later
List_Of_Queries <- Filter(function(x) dim(x)[1], List_Of_Queries)
#Merchdescr to discover online trans| See inside immediate for loop for specific search parameters
#State Trans to determine B&M| See inside immediate for loop for specific search parameters

#IMPORTANT
#If a brand has ZERO trans attached to it by Kevin's model and our ruling so far, this will crash. Ensure you that doesn't happen.
#Begin looking for Online and B&M components. Separate out easy stuff and look for potential store rules.
Online_Additions <- NULL
Store_Additions <- NULL
Maxed_Checks <- NULL
for(i in 1:length(List_Of_Queries)){
    Total_Options <- nrow(List_Of_Queries[[i]])
    ONLINE_First_Look <- round(nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$merchdesc,gregexpr("^\\s*[a-zA-Z]+\\s*\\*.*",List_Of_Queries[[i]]$merchdesc)))))/Total_Options,2)
    ONLINE_Second_Look <- round(nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$merchdesc,gregexpr("\\.[Cc][Oo][Mm]",List_Of_Queries[[i]]$merchdesc)))))/Total_Options,2)
    ONLINE_Third_Look <- round(nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$merchdesc,gregexpr("\\.[Aa][Ii]$",List_Of_Queries[[i]]$merchdesc)))))/Total_Options,2)
    ONLINE_Fourth_Look <- round(nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$merchdesc,gregexpr(".*[Ss][Uu][Bb][Ss][Cc][Rr][Ii][Pp][Tt].*",List_Of_Queries[[i]]$merchdesc)))))/Total_Options,2)
    ONLINE_Fifth_Look <- round(nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$brand_name,gregexpr("\\.COM",List_Of_Queries[[i]]$brand_name)))))/Total_Options,2)
    ONLINE_Sixth_Look <- round(nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$merchdesc,gregexpr(".*DOORDASH.*",List_Of_Queries[[i]]$merchdesc)))))/Total_Options,2)
    ONLINE_Seventh_Look <- round(nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$merchdesc,gregexpr("^ORDERUP.*",List_Of_Queries[[i]]$merchdesc)))))/Total_Options,2)
    ONLINE_Eighth_Look <- round(nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$merchdesc,gregexpr(".*EAT24.*",List_Of_Queries[[i]]$merchdesc)))))/Total_Options,2)
    ONLINE_Ninth_Look <- round(nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$merchdesc,gregexpr(".*GRUBHUB.*",List_Of_Queries[[i]]$merchdesc)))))/Total_Options,2)
    ONLINE_Tenth_Look <- round(nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$merchdesc,gregexpr(".*SEAMLESS.*",List_Of_Queries[[i]]$merchdesc)))))/Total_Options,2)
    ONLINE_Eleventh_Look <- round(nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$merchdesc,gregexpr(".*POSTMATES.*",List_Of_Queries[[i]]$merchdesc)))))/Total_Options,2)
    
    ONLINE_Looks <- as.data.frame(c(ONLINE_First_Look,
                                    ONLINE_Second_Look,
                                    ONLINE_Third_Look,
                                    ONLINE_Fourth_Look,
                                    ONLINE_Fifth_Look,
                                    ONLINE_Sixth_Look,
                                    ONLINE_Seventh_Look,
                                    ONLINE_Eighth_Look,
                                    ONLINE_Ninth_Look,
                                    ONLINE_Tenth_Look,
                                    ONLINE_Eleventh_Look
    ))
    
    colnames(ONLINE_Looks) <- c("Checks")
    ONLINE_Looks$Rule <- c("\\s*[a-zA-Z]+\\s*\\*.*", "\\.[Cc][Oo][Mm]","\\.[Aa][Ii]$",".*[Ss][Uu][Bb][Ss][Cc][Rr][Ii][Pp][Tt].*","\\.COM",".*DOORDASH.*","^ORDERUP.*",".*EAT24.*", ".*GRUBHUB.*",".*SEAMLESS.*",".*POSTMATES.*")
    ONLINE_Looks$Brand <- List_Of_Queries[[i]]$brand_name[1]
    ONLINE_Looks$Checks_NA <- ifelse(ONLINE_Looks$Checks <=.05, NA, ONLINE_Looks$Checks)
    List_Of_Queries[[i]]$ONLINE_Check <- ifelse((mean(ONLINE_Looks$Checks_NA, na.rm = T) > .10) | max(ONLINE_Looks$Checks, na.rm = T) > .25, "ONLINE", NA)
    ONLINE_Rules <- ONLINE_Looks[which(ONLINE_Looks$Checks > 0),]
    ONLINE_Rules <- ONLINE_Rules[,-ncol(ONLINE_Rules)]
    ONLINE_Rules <- ONLINE_Rules[which(ONLINE_Rules$Checks > .05),]
    Online_Additions <- rbind(Online_Additions, ONLINE_Rules)
    
    
    Max_Online <- ONLINE_Looks[which.max(ONLINE_Looks$Checks),]
    Max_Online <- Max_Online[which(Max_Online$Checks > 0),]
    Max_Online <- Max_Online[,-ncol(Max_Online)]
    Maxed_Checks <- rbind(Maxed_Checks, Max_Online)
    
    Store_Subset <- as.data.frame(List_Of_Queries[[i]]$merchdesc)
    #I hate phone Numbers & Addresses!
    if(nrow(as.data.frame(Store_Subset[-grep("\\d{8}\\d*", Store_Subset$`List_Of_Queries[[i]]$merchdesc`),])) != 0){
        Store_Subset <- as.data.frame(Store_Subset[-grep("\\d{8}\\d*", Store_Subset$`List_Of_Queries[[i]]$merchdesc`),])
        colnames(Store_Subset) <- c("List_Of_Queries[[i]]$merchdesc")
    }else{
        Store_Subset <- Store_Subset
    }
    if(nrow(as.data.frame(Store_Subset[-grep("\\(\\d{3}\\)", Store_Subset$`List_Of_Queries[[i]]$merchdesc`),])) != 0){
        Store_Subset <- as.data.frame(Store_Subset[-grep("\\(\\d{3}\\)", Store_Subset$`List_Of_Queries[[i]]$merchdesc`),])
        colnames(Store_Subset) <- c("List_Of_Queries[[i]]$merchdesc")
    }else{
        Store_Subset <- Store_Subset
    }
    if(nrow(as.data.frame(Store_Subset[-grep("\\d{3}\\-\\d{3}", Store_Subset$`List_Of_Queries[[i]]$merchdesc`),])) != 0){
        Store_Subset <- as.data.frame(Store_Subset[-grep("\\d{3}\\-\\d{3}", Store_Subset$`List_Of_Queries[[i]]$merchdesc`),])
        colnames(Store_Subset) <- c("List_Of_Queries[[i]]$merchdesc")
    }else{
        Store_Subset <- Store_Subset
    }
    colnames(Store_Subset) <- c("Merchdesc")
    First_Store_Check <- nrow(as.data.frame(unlist(regmatches(Store_Subset$Merchdesc,gregexpr(paste(List_Of_Queries[[i]]$brand_name[i],"\\s*\\d{4}$",sep = ""),Store_Subset$Merchdesc)))))
    Second_Store_Check <- nrow(as.data.frame(unlist(regmatches(Store_Subset$Merchdesc,gregexpr(paste(List_Of_Queries[[i]]$brand_name[i],"\\s*\\d{3}$",sep = ""),Store_Subset$Merchdesc)))))
    Third_Store_Check <- nrow(as.data.frame(unlist(regmatches(Store_Subset$Merchdesc,gregexpr(paste(List_Of_Queries[[i]]$brand_name[i],".*\\-+\\s+\\d{2}\\d*",sep = ""),Store_Subset$Merchdesc)))))
    Fourth_Store_Check <- nrow(as.data.frame(unlist(regmatches(Store_Subset$Merchdesc,gregexpr(paste(List_Of_Queries[[i]]$brand_name[i],"^\\d{4}.*",sep = ""),Store_Subset$Merchdesc)))))
    Fifth_Store_Check <-nrow(as.data.frame(unlist(regmatches(Store_Subset$Merchdesc,gregexpr(paste(List_Of_Queries[[i]]$brand_name[i],"^\\d{3}\\s+",sep = ""),Store_Subset$Merchdesc)))))
    Sixth_Store_Check <- nrow(as.data.frame(unlist(regmatches(Store_Subset$Merchdesc,gregexpr(paste(List_Of_Queries[[i]]$brand_name[i],".*\\s*\\#+\\s*\\d{1}\\d*",sep = ""),Store_Subset$Merchdesc)))))
    Seventh_Store_Check <- nrow(as.data.frame(unlist(regmatches(Store_Subset$Merchdesc,gregexpr(paste(List_Of_Queries[[i]]$brand_name[i],"\\s*\\d{5}$",sep = ""),Store_Subset$Merchdesc)))))
    
    
    List_Of_Queries[[i]]$Store_Check <- ifelse(round(sum(First_Store_Check,Second_Store_Check,Third_Store_Check,Fourth_Store_Check,Fifth_Store_Check,Sixth_Store_Check,Seventh_Store_Check)/Total_Options,4) > .15, "Likely", "Unlikely") 
    
    Store_Looks <- as.data.frame(c(
        First_Store_Check,
        Second_Store_Check,
        Third_Store_Check,
        Fourth_Store_Check,
        Fifth_Store_Check,
        Sixth_Store_Check,
        Seventh_Store_Check
    ))
    colnames(Store_Looks) <- c("Checks")
    Store_Looks$Rules <- c(paste(List_Of_Queries[[i]]$brand_name[i],"\\s*\\d{4}$",sep = ""), 
                           paste(List_Of_Queries[[i]]$brand_name[i],"\\s*\\d{3}$",sep = ""), 
                           paste(List_Of_Queries[[i]]$brand_name[i],".*\\-+\\s+\\d{2}\\d*",sep = ""), 
                           paste(List_Of_Queries[[i]]$brand_name[i],"^\\d{4}.*",sep = ""), 
                           paste(List_Of_Queries[[i]]$brand_name[i],"^\\d{3}\\s+",sep = ""), 
                           paste(List_Of_Queries[[i]]$brand_name[i],".*\\s*\\#+\\s*\\d{1}\\d*",sep = ""),
                           paste(List_Of_Queries[[i]]$brand_name[i],"\\s*\\d{5}$",sep = ""))
    
    Store_Looks$Desired_Element <- c("\\d{4}","\\d{3}", "\\d{2}\\d*", "^\\d{4}", "^\\d{3}", "\\d{1}\\d*","\\d{5}")
    Store_Looks$Brand <- List_Of_Queries[[i]]$brand_name[1]
    Store_Looks$Percent <- round(Store_Looks$Checks / Total_Options,2)
    Store_Looks <- Store_Looks[which.max(Store_Looks$Percent),]
    Store_Looks <- Store_Looks[which(Store_Looks$Percent > .05),]
    
    Store_Additions <- rbind(Store_Additions,Store_Looks)
    
    List_Of_Queries[[i]]$`B&M_Check` <- ifelse(((List_Of_Queries[[i]]$state == List_Of_Queries[[i]]$top_state_abbr) & (mean(List_Of_Queries[[i]]$pct_state_trans) >= .50)) | (mean(List_Of_Queries[[i]]$pct_state_trans) >= .80),"Likely","Unlikely")
    List_Of_Queries[[i]]$`B&M_Check`[is.na(List_Of_Queries[[i]]$`B&M_Check`)] <- "Unlikely"
    Likely_Check <-  nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$`B&M_Check`,gregexpr("Likely",List_Of_Queries[[i]]$`B&M_Check`)))))
    Unlikely_Check <-  nrow(as.data.frame(unlist(regmatches(List_Of_Queries[[i]]$`B&M_Check`,gregexpr("Unlikely",List_Of_Queries[[i]]$`B&M_Check`)))))
    List_Of_Queries[[i]]$`B&M_Check` <- ifelse(Likely_Check >= Unlikely_Check, "B&M", NA)
    List_Of_Queries[[i]]$`B&M_Check` <- ifelse(List_Of_Queries[[i]]$Store_Check == "Likely", "B&M",List_Of_Queries[[i]]$`B&M_Check` )
    
    Drop_Missing_States <- na.omit(data.frame(List_Of_Queries[[i]]$merchdesc,List_Of_Queries[[i]]$state,List_Of_Queries[[i]]$top_state_abbr))
    colnames(Drop_Missing_States) <- c("Brand","Origin","Top_State")
    Drop_Missing_States$Match <- ifelse(as.character(Drop_Missing_States$Origin) == as.character(Drop_Missing_States$Top_State),1,0)
    List_Of_Queries[[i]]$`B&M_Check` <- ifelse((nrow(Drop_Missing_States)>3)&((sum(Drop_Missing_States$Match)/nrow(Drop_Missing_States))<.50) &(is.na(unique(List_Of_Queries[[i]]$ONLINE_Check)) == T),"Odd_State_Sales", List_Of_Queries[[i]]$`B&M_Check`)  
    List_Of_Queries[[i]]$`B&M_Check` <- ifelse((nrow(List_Of_Queries[[i]]) < 7) & (is.na(List_Of_Queries[[i]]$ONLINE_Check) == T) , "Odd_State_Sales", List_Of_Queries[[i]]$`B&M_Check`)
}

#Just cleaning up our results into a clean and unified location in the following 'for' loop
Unique_Brands <- NULL
for(i in 1:length(List_Of_Queries)){
    Determinations <- data.frame(unique(List_Of_Queries[[i]]$brand_id),unique(List_Of_Queries[[i]]$brand_name) ,unique(List_Of_Queries[[i]]$ONLINE_Check), unique(List_Of_Queries[[i]]$`B&M_Check`),unique(List_Of_Queries[[i]]$Store_Check))
    colnames(Determinations) <- c("Brand_ID","Brand","ONLINE_Channel","B&M_Channel","Store_Locations")
    Unique_Brands <- rbind(Unique_Brands, Determinations)
}
#Retrieve My Scraped Channel Assignments#
#Here we set an initial df for later fullfillment and ping my droplet to spin up my docker hub
Combined_Discoveries <- NULL
remDr = remoteDriver(remoteServerAddr = "{your_ip}", port = 4445L, browser = "chrome")
remDr$open()
#Line 183 allows for quick and easy spot check to double check why (or why not) certain brands were flagged
#Scrape Narration (Imagine a charismatic Morgan Freeman):
# Hello Google, please search for this brand name.
# Huzzah! Results found! Please click that top link (NOT the advertisement)
# Google you treat us so well, never change (I know, Morgan deserves better writing)
# Alright! Now that we're on the brands main webpage, pull their ENTIRE SITES HTML
# Now that we have all their secret, let's dig for our indicates (See inside the loop for specific parameters)
# These brands are not web designers, their sites follow a great deal of consistency, giving us all the power (Cue meme?)
# If ANY of our parameters trigger for either ONLINE or B&M, flagged that brand appropriately
# And we're done.
for (param in og_list$brand){
    Sys.sleep(1.25)
    remDr$navigate("https://www.google.com")
    webElem <- remDr$findElement(using = 'name', value = "q")
    webElem$sendKeysToElement(list(param, key = "enter"))
    Sys.sleep(.25)
    First_Link <- remDr$findElement(using = 'class', value = "DKV0Md")
    First_Link$clickElement()
    tryCatch(expr = {First_Link$clickElement()}, error = function(e){print("No Need for Double Click")})
    Sys.sleep(.25)
    #remDr$acceptAlert()
    Contents <- as.character(read_html(unlist(remDr$getPageSource())))
    Page_Contents <- toupper(Contents)
    First_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*STORE\\s+INFO.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    Second_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*STORE\\s+LOCATOR.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    Third_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*\\s+HOTEL\\s+.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    Fourth_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*\\s+PICK\\-*UP.*DELIVERY.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    Fifth_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*STATION\\-FINDER.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    Sixth_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*\\/LOCATIONS.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    Seventh_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*FIND\\s+US.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    Eighth_Search <-ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*LOCATIONS\\..*\\.COM\\/SEARCH.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    Ninth_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*STORES-DEALERS.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    Tenth_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*FIND\\s+A\\s+STORE.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    Eleventh_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*SHOW\\s*ROOM.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    Twelfth_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*RETAIL\\s*STORE.*",Page_Contents)))) == 0, "EMPTY", "B&M FOUND")
    
    Brick_Searches <-paste(First_Search,
                           Second_Search,
                           Third_Search,
                           Fourth_Search,
                           Fifth_Search,
                           Sixth_Search,
                           Seventh_Search,
                           Eighth_Search,
                           Ninth_Search,
                           Tenth_Search,
                           Eleventh_Search,
                           Twelfth_Search,
                           sep=" ")
    
    Brick_Discovery <- ifelse(length(unlist(regmatches(Brick_Searches,gregexpr("B&M FOUND*",Brick_Searches)))) == 0, NA, "B&M Element")
    
    
    First_Online_Search <- tryCatch( expr = {unlist(remDr$findElement("id","shop")$getElementText())}, error = function(e){First_Online_Search <- "EMPTY"})
    First_Online_Search <- ifelse(First_Online_Search != "EMPTY", "ONLINE SALES", First_Online_Search)
    Second_Online_Search <- tryCatch( expr = {unlist(remDr$findElement("id","product")$getElementText())}, error = function(e){Second_Online_Search <- "EMPTY"})
    Second_Online_Search <- ifelse(Second_Online_Search != "EMPTY", "ONLINE SALES", Second_Online_Search)
    Third_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*ORDERCALCULATE.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Fourth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*SALE.HTML.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Fifth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*MY ACCOUNT.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Sixth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*LOG IN.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Seventh_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*ONLINE ORDER.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Eighth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*DIGITAL SHOPPING.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Ninth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*CYBER.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Tenth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*SHOPPING BAG.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Eleventh_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*MEAL PLAN.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Twelfth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*HEADER-CART-LINK.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Thirteenth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*TRACK ORDER.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Fourteenth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*ICON-CART.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Fifthteenth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*VIEW CART.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Sixteenth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*ENERGY SERVICE.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Seventhteenth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*MY CART.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Eightteenth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*ENERGY PROCUREMENT.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Nineteenth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*\\/CART.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Twentieth_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*ORDER\\s*ONLINE.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Twenone_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*DELIVERY.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    Twentwo_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*NAV\\-SHOP\\-LINK.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    TwenThree_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*GET A DEMO.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    TwenFour_Online_Search <- ifelse(length(unlist(regmatches(Page_Contents,gregexpr(".*///SHOP///.*",Page_Contents)))) == 0, "EMPTY", "ONLINE SALES")
    
    Online_Searches <- paste(
        First_Online_Search,
        Second_Online_Search,
        Third_Online_Search,
        Fourth_Online_Search,
        Fifth_Online_Search,
        Sixth_Online_Search,
        Seventh_Online_Search,
        Eighth_Online_Search,
        Ninth_Online_Search,
        Tenth_Online_Search,
        Eleventh_Online_Search,
        Twelfth_Online_Search,
        Thirteenth_Online_Search,
        Fourteenth_Online_Search,
        Fifthteenth_Online_Search,
        Sixteenth_Online_Search,
        Seventhteenth_Online_Search,
        Eightteenth_Online_Search,
        Nineteenth_Online_Search,
        Twentieth_Online_Search,
        Twenone_Online_Search,
        Twentwo_Online_Search,
        TwenThree_Online_Search,
        TwenFour_Online_Search,
        sep = " ")
    
    Online_Discovery <- ifelse(length(unlist(regmatches(Online_Searches,gregexpr("ONLINE SALES",Online_Searches)))) == 0, NA, "Online Element")
    
    Discoveries <- cbind(param, Brick_Discovery,Online_Discovery)
    Combined_Discoveries <- rbind(Combined_Discoveries, Discoveries)
    
}
Combined_Discoveries <- as.data.frame(Combined_Discoveries)
Combined_Discoveries$Channel_Assignment <- ifelse(is.na(Combined_Discoveries$Brick_Discovery) == F & is.na(Combined_Discoveries$Online_Discovery) == F, "NO DEFAULT", "ONLINE")
Combined_Discoveries$Channel_Assignment <- ifelse((Combined_Discoveries$Channel_Assignment == "ONLINE") & is.na(Combined_Discoveries$Brick_Discovery) == F, "B&M", Combined_Discoveries$Channel_Assignment)
Combined_Discoveries$Channel_Assignment <- ifelse(is.na(Combined_Discoveries$Brick_Discovery) == T & is.na(Combined_Discoveries$Online_Discovery) == T, NA, Combined_Discoveries$Channel_Assignment)
#Now we just merge our scraper results with our internal data assignments
Unique_Brands$Scraped_Channel <- Combined_Discoveries$Channel_Assignment
# Lines 295-299 only necessary if we ran scrape for everything all at once (ill advised)
# drive_auth(email = "cpachulski@cei.ventures", use_oob=TRUE)
# gs4_auth(email = "cpachulski@cei.ventures", use_oob=TRUE)
# ss <- drive_get("Channel Review")
# Scraped_Channels <- range_read(ss, sheet = "2020-04-15 Channel_Review")
# Unique_Brands$Scraped_Channel <- Scraped_Channels$Brand_Results.Verdict[match(Unique_Brands$Brand,Scraped_Channels$Brand_Results.Brand.Name)]
# BASE LAYER Verdict Assignment#
Unique_Brands$Final_Verdict <- NA
#ALL are NA's. They're all teenagers, nobody knows what they're going to be, but they're awaiting our societal judgment. 
Unique_Brands$Final_Verdict <- ifelse((is.na(Unique_Brands$Final_Verdict) == T) & (Unique_Brands$ONLINE_Channel == "ONLINE"), "ONLINE", NA)
# Check for inital Online
Unique_Brands$Final_Verdict <- ifelse((is.na(Unique_Brands$Final_Verdict) == T) & (Unique_Brands$`B&M_Channel` == "B&M"), "B&M", Unique_Brands$Final_Verdict)
# If not Online, check for B&M
Unique_Brands$Final_Verdict <- ifelse((Unique_Brands$`B&M_Channel` == "Odd_State_Sales")&is.na(Unique_Brands$ONLINE_Channel == T), "NO DEFAULT", Unique_Brands$Final_Verdict )
# Large B&M brands span a lot of states and often spring up online purchasing abilities, lets account for that here
Unique_Brands$Final_Verdict <- ifelse((Unique_Brands$Store_Locations== "Likely"), "B&M", Unique_Brands$Final_Verdict)
# channel assignment viewing internal data only (slight splash of Scrape, but very minimal).
Unique_Brands$New <- ifelse((Unique_Brands$`B&M_Channel` == "Odd_State_Sales") & (Unique_Brands$ONLINE_Channel == "ONLINE"), "ONLINE", Unique_Brands$Final_Verdict)
Unique_Brands$New <- ifelse(is.na(Unique_Brands$New == F) & (Unique_Brands$New != Unique_Brands$Final_Verdict), Unique_Brands$Final_Verdict, Unique_Brands$New)
Unique_Brands$New <- ifelse((Unique_Brands$`B&M_Channel` == "Odd_State_Sales") & (Unique_Brands$ONLINE_Channel == "ONLINE"), "ONLINE", Unique_Brands$Final_Verdict)
Unique_Brands$New <- ifelse(is.na(Unique_Brands$New == F) & (Unique_Brands$New != Unique_Brands$Final_Verdict), Unique_Brands$Final_Verdict, Unique_Brands$New)
Unique_Brands$New <- ifelse((Unique_Brands$Store_Locations== "Likely") & (Unique_Brands$Scraped_Channel == "ONLINE" ),"NO DEFAULT", Unique_Brands$Final_Verdict  )
Unique_Brands$New <- ifelse(is.na(Unique_Brands$New == F) & (Unique_Brands$New != Unique_Brands$Final_Verdict), Unique_Brands$Final_Verdict, Unique_Brands$New)
Unique_Brands$New <- ifelse(Unique_Brands$`B&M_Channel` == "Odd_State_Sales" & Unique_Brands$Final_Verdict == "B&M", "NO DEFAULT",Unique_Brands$Final_Verdict == "B&M" )
Unique_Brands$New <- ifelse((Unique_Brands$New == "NO DEFAULT") & (Unique_Brands$New != Unique_Brands$Final_Verdict), Unique_Brands$New, Unique_Brands$Final_Verdict)
Unique_Brands$Final_Verdict <- Unique_Brands$New
# So, if we have odd state sales, we assume* that you are a multi state brand. 
# If you were B&M, we change that to be online given this immediate prior assumption.
# EVEN IF a brand HAD STORE NUMBERS, we will assume NO DEFAULT given the multi state nature of the brand.
Unique_Brands$Final_Verdict <- ifelse(is.na(Unique_Brands$New == T), Unique_Brands$Final_Verdict, Unique_Brands$New)
Unique_Brands$Final_Verdict <- ifelse((is.na(Unique_Brands$Final_Verdict) == T), Unique_Brands$Scraped_Channel, Unique_Brands$Final_Verdict)

# SCRAPER DEFERMENT Verdict Assignment#
# Through testing, my scraper is a lot more accurate than what we're able to glean from internal data 
# Bc of this, the following rules create "deferment" conditions to pass/fail against my scrape results
Unique_Brands$New <- ifelse((Unique_Brands$ONLINE_Channel == "ONLINE") & (Unique_Brands$`B&M_Channel` == "B&M") & is.na(Unique_Brands$Scraped_Channel) == F, "NO DEFAULT", Unique_Brands$Final_Verdict)
Unique_Brands$New <- ifelse(is.na(Unique_Brands$New)==T, Unique_Brands$Final_Verdict, Unique_Brands$New)
Unique_Brands$Final_Verdict <- Unique_Brands$New
# If both our internal criteria's fail and* we don't have a scraped determinate, assign no default to be safe.
Unique_Brands$New<- ifelse((Unique_Brands$Scraped_Channel == "NO DEFAULT"), "NO DEFAULT", Unique_Brands$Final_Verdict)
Unique_Brands$New <- ifelse(is.na(Unique_Brands$New)==T, Unique_Brands$Final_Verdict, Unique_Brands$New)
Unique_Brands$Final_Verdict <- Unique_Brands$New
# Just because State %'s were parallel, default to scraper for B&M existance
Unique_Brands$New <- ifelse(((Unique_Brands$`B&M_Channel` == "B&M")& (Unique_Brands$Scraped_Channel == "ONLINE")), "ONLINE", Unique_Brands$Final_Verdict)
Unique_Brands$New <- ifelse(is.na(Unique_Brands$New)==T, Unique_Brands$Final_Verdict, Unique_Brands$New)
Unique_Brands$Final_Verdict <- Unique_Brands$New

# If we couldn't find online payment systems but recognized odd state sales and the scraper found no B&M components, reach logical conclusion
Unique_Brands$New <- ifelse(((Unique_Brands$`B&M_Channel` == "Odd_State_Sales")&(Unique_Brands$Scraped_Channel == "ONLINE")), "ONLINE", Unique_Brands$Final_Verdict)
Unique_Brands$New <- ifelse(is.na(Unique_Brands$New)==T, Unique_Brands$Final_Verdict, Unique_Brands$New)
Unique_Brands$Final_Verdict <- Unique_Brands$New

# Defer to scraper for disagreements
Unique_Brands$New <- ifelse(((Unique_Brands$ONLINE_Channel == "ONLINE")&(Unique_Brands$Scraped_Channel == "NO DEFAULT")), "NO DEFAULT", Unique_Brands$Final_Verdict)
Unique_Brands$New <- ifelse(is.na(Unique_Brands$New)==T, Unique_Brands$Final_Verdict, Unique_Brands$New)
Unique_Brands$Final_Verdict <- Unique_Brands$New

#Defer to scraper for online confirmation
Unique_Brands$New <- ifelse((Unique_Brands$ONLINE_Channel == "ONLINE") & (Unique_Brands$`B&M_Channel` == "B&M") & (Unique_Brands$Scraped_Channel == "ONLINE"), "ONLINE", Unique_Brands$Final_Verdict)
Unique_Brands$New <- ifelse(is.na(Unique_Brands$New)==T, Unique_Brands$Final_Verdict, Unique_Brands$New)
Unique_Brands$Final_Verdict <- Unique_Brands$New

#Executive Decision <- Brand Titles that Indicate Certain Results#
# In our initial internal channel assignment, I kept track of all the params we used to "assign" channel values
# They'd make for very helpful rules. So let's, now that we have our final channel verdict reached, drop the matching channels and assign rules to brands
# Eg. We don't need ONLINE rules for an ONLINE brand, so don't bother analyzing for it.
Online_Additions$Brand_ID <- Unique_Brands$Brand_ID[match(Online_Additions$Brand,Unique_Brands$Brand)]
Store_Additions$Brand_ID <- Unique_Brands$Brand_ID[match(Store_Additions$Brand,Unique_Brands$Brand)]
Online_Additions$Brand_ID <- paste("https://internals.consumer-edge.com/midchannelpattern/add?brand_id=",Online_Additions$Brand_ID,sep="")
Store_Additions$Brand_ID <- paste("https://internals.consumer-edge.com/storenumpattern/add?brand_id=",Store_Additions$Brand_ID,sep="")
# Real quick, lets add links to our internal site so that we don't have to wait 30+ seconds for all the brands to load later 
Online_Additions$Channel_Verdict <- Unique_Brands$Final_Verdict[match(Online_Additions$Brand,Unique_Brands$Brand)]
Online_Additions$Channel_Verdict <- ifelse(Online_Additions$Channel_Verdict == "Online" | Online_Additions$Channel_Verdict == "ONLINE", NA, Online_Additions$Channel_Verdict)
Online_Additions <- na.omit(Online_Additions)
Online_Additions <- Online_Additions[which(Online_Additions$Checks >= .10),]
Store_Additions$Channel_Verdict <- Unique_Brands$Final_Verdict[match(Store_Additions$Brand,Unique_Brands$Brand)]
#Store_Additions$Channel_Verdict <- ifelse(Store_Additions$Channel_Verdict == "Online" | Store_Additions$Channel_Verdict == "ONLINE", NA, Store_Additions$Channel_Verdict)
Store_Additions <- na.omit(Store_Additions)
Store_Additions <- Store_Additions[which(Store_Additions$Percent > .03),]

Unique_Brands$Online_Makeup <- Maxed_Checks$Checks[match(Unique_Brands$Brand,Maxed_Checks$Brand)]

Unique_Brands$New <- ifelse(Unique_Brands$Online_Makeup >= .85, "ONLINE", Unique_Brands$Final_Verdict)
Unique_Brands$New <- ifelse(Unique_Brands$New != Unique_Brands$Final_Verdict, Unique_Brands$New,Unique_Brands$Final_Verdict)
Unique_Brands$New <- ifelse(is.na(Unique_Brands$New)==T, Unique_Brands$Final_Verdict, Unique_Brands$New)
Unique_Brands$Final_Verdict <- Unique_Brands$New
# One final check on the verdict. And this is me feeling frisky.
# If we have a single rule. ONE RULE. That maps over 75% pf a brand as default online, ignore all my other prior assignment and make it default Online.


Unique_Brands <- Unique_Brands[-ncol(Unique_Brands)]
Unique_Brands <- Unique_Brands[-ncol(Unique_Brands)]
Unique_Brands$Links <- paste("https://internals.consumer-edge.com/brand/detail/", Unique_Brands$Brand_ID,sep="")

# This loop is where the majority of DANGER comes in, but it's necessary, in my eyes. 
# There are certain brands and companies that WE KNOW how they should be assigned given the nature of our data
# So, throw out all prior analysis for these brands and assign what we know to be true.
for(i in 1:nrow(Unique_Brands)){
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*GAS$",as.character(Unique_Brands[i,2])))) == as.character(Unique_Brands[i,2]), "Online", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*AIRLINE\\s*.*",Unique_Brands[i,2])))== as.character(Unique_Brands[i,2]), "NO DEFAULT", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*AIR\\s*.*",Unique_Brands[i,2])))== as.character(Unique_Brands[i,2]), "NO DEFAULT", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr("^AIR\\s+\\.*",as.character(Unique_Brands[i,2]))))== as.character(Unique_Brands[i,2]), "NO DEFAULT", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*INN\\s*.*",as.character(Unique_Brands[i,2]))))== as.character(Unique_Brands[i,2]), "NO DEFAULT", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*SUITE\\s*.*",as.character(Unique_Brands[i,2]))))== as.character(Unique_Brands[i,2]), "NO DEFAULT", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*HOTEL\\s*.*",as.character(Unique_Brands[i,2]))))== as.character(Unique_Brands[i,2]), "NO DEFAULT", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*RESORTS\\s.*",as.character(Unique_Brands[i,2]))))== as.character(Unique_Brands[i,2]), "NO DEFAULT", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*RESORT\\s*.*",as.character(Unique_Brands[i,2]))))== as.character(Unique_Brands[i,2]), "NO DEFAULT", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*CRUISE\\s*.*",as.character(Unique_Brands[i,2]))))== as.character(Unique_Brands[i,2]), "NO DEFAULT", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*SOFTWARE.*",as.character(Unique_Brands[i,2]))))== as.character(Unique_Brands[i,2]), "ONLINE", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*TRAVEL.*",as.character(Unique_Brands[i,2]))))== as.character(Unique_Brands[i,2]), "ONLINE", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*\\.COM.*",as.character(Unique_Brands[i,2]))))== as.character(Unique_Brands[i,2]), "ONLINE", Unique_Brands[i,7])
    Unique_Brands[i,7] <- ifelse((regmatches(as.character(Unique_Brands[i,2]),gregexpr(".*\\s*OIL$",as.character(Unique_Brands[i,2]))))== as.character(Unique_Brands[i,2]), "ONLINE", Unique_Brands[i,7])
    
}

Unique_Brands$Final_Verdict[is.na(Unique_Brands$Final_Verdict)==T] <- "NO DEFAULT"
# Writing a rule for airlines, gyms, Resorts, hotels, suites, Inn,  INSURANCE, Energy
#Export List for Manual Review#
ss <- drive_get("Query_Exports_From_R")
Unique_Brands$Change_Made <- ifelse(Unique_Brands$Final_Verdict == "NO DEFAULT", "N","Y")
Unique_Brands$Links <- paste("https://internals.consumer-edge.com/brand/detail/",Unique_Brands$Brand_ID,sep="")
#sheets_deauth()
gs4_auth(email = "cpachulski@cei.ventures")
currentDate <- Sys.Date()
sheet_write(
    Unique_Brands,
    ss = ss,
    sheet = paste(currentDate,"_Assignments",sep="")
)



#Enter Channel Assignments - adjust subindustries as needed#
remDr$open()
remDr$maxWindowSize()
remDr$navigate("https://internals.consumer-edge.com/auth/login?next=%2Fsymbol%2Flist")
Sys.sleep(2)
username <- remDr$findElement(using = "id", value = "username")
username$clearElement()
username$sendKeysToElement(list("cpachulski@consumer-edge.com"))

passwd <- remDr$findElement(using = "id", value = "password")
passwd$clearElement()
passwd$sendKeysToElement(list("{your_password}"))

Post_Credential_Login <- remDr$findElement(using = "id", value = "submit")
Post_Credential_Login$submitElement()
Sys.sleep(2)

i = 1
for(i in 1:nrow(Unique_Brands)){
    Sys.sleep(sample(1:7, 1))
    remDr$navigate(Unique_Brands$Links[i])
    Sys.sleep(sample(1:4,1))
    # Sys.sleep(sample(20))
    # Brand_Idenitfied <- remDr$findElement(using = "class", value = "sorting_1")$getElementText()
    
    # Search_Option <- remDr$findElement(using = "css selector", value = "[type='search']")
    # Search_Option$sendKeysToElement(list(param))
    
    URL <- paste("https://internals.consumer-edge.com/brand/detail/", as.character(Unique_Brands$Brand_ID),sep="")
    remDr$navigate(URL)
    #Edit_Button <- paste("/brand/edit/",Brand_Idenitfied,sep="")
    
    Edit_Details <- remDr$findElement(using = "link text", value = "Edit Details")$clickElement()
    Sys.sleep(.35)
    Default_Subchannel <- remDr$findElement(using = "id", value = "default_subchannel")$clickElement()
    Default_Subchannel <- remDr$findElements("css","#default_subchannel option")
    #NO Default is [[1]]
    #   #Online is [[2]]
    #   #Brick & Mortar is [[3]]
    #   #Phone is [[4]]
    #   #Catalog is [[5]]
    Verdict <- Unique_Brands$Final_Verdict[i]
    if(Verdict == "ONLINE"){
        Default_Subchannel[[2]]$clickElement()
    } else if (Verdict == "B&M"){
        Default_Subchannel[[3]]$clickElement()
    } else {Default_Subchannel[[1]]$clickElement()}
    #Default_Subchannel[[1]]$clickElement()
    Verdict_Text <- as.character(if(Verdict == "ONLINE"){
        Default_Subchannel[[2]]$getElementText()
    } else if (Verdict == "B&M"){
        Default_Subchannel[[3]]$getElementText()
    } else {Default_Subchannel[[1]]$getElementText()})
    
    Sys.sleep(.35)
    Default_Channel_Publish <- remDr$findElement(using = "id", value = "channel_publish_flag")$clickElement()
    Default_Channel_Publish <- remDr$findElements("css","#channel_publish_flag option")
    
    if(Verdict_Text == ""){
        Default_Channel_Publish[[2]]$clickElement()
    } else {Default_Channel_Publish[[2]]$clickElement()}
    Sys.sleep(.35)
    # No is [[1]]
    # Yes is [[2]]
    
    webElem <- remDr$findElement("xpath", "/html/body/div[2]/div/a")
    
    webElem$sendKeysToElement(list(key = "control", "t"))
    
    Sys.sleep(.35)
    # Onclick_Pop_Up <- remDr$findElement("class", "form-control")
    Submission <- remDr$findElement("id", "submit")
    Submission$clickElement()
    #   
    remDr$acceptAlert()
}

#CEI Suggested Store Rules - General Check####
Store_Additions

#Daily Review of Brand Tagging####
List_Of_Queries = current_brand_tagging("BiP",paste(floor_date(today(),"week",1),"_Newbies",sep=""))
i = 1
List_Of_Queries[[i]]
i#%>% filter(mcc_rec_rem == "remove") %>% distinct(mcc) %>% filter(mcc >= 7300)
i = i + 1
