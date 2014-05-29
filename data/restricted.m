format long
for i=1:(length(post))
    filename=char(post(i));
    
    file=fopen(filename,'r');
    
    varname=strrep(filename,'.txt','');
    
    file_data=fscanf(file,'%f');
    
    outfilename=strcat(varname, '_restrict.txt');
    
    fileout=fopen(outfilename,'w');
    
    for j=1:length(file_data)
        if(-1 < file_data(j,1) && file_data(j,1) < 4)
            fprintf(fileout, '%f\n', file_data(j,1));
        end
    end
    
    
    
    
    
    
    
    
    
    
    
    
    
end