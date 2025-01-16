%% Radiative Forcing 
% Anne Martine Wilce, 2023
% Adapted from Shannon Healy's R version 

% This code is intended to be used to calculate instantaneous radiative
% forcing values from albedo values. 
% In the future it could be expanded to calculate radiative forcing over a
% given time period (which is dependent on the location of the site).


%% Input file selection

% You can use either a hard coded path for your input file here (good for testing
% environment or debugging) or comment
% these two lines out and have a GUI pop up. 

clear %clear workspace

file2read = '/Users/alia/Library/Mobile Documents/com~apple~CloudDocs/Documents/Manuscripts - In Prep/Manuscripts - Lemon Creek 2023/SNICAR/RadForce/20240205_LC_algae_only_snicar_output copy.xlsx'; %manual or GUI option for read-in
%IF USING THIS METHOD, CHANGE YOUR PATH TO THE FOLDER YOUR INPUT IS IN FOR
%BEST USE

if  ~exist('file2read','var')
    beep;fprintf('Select the albedo .xslx data file to process:\n')
    pause(2)
    [file2read, filepath]=  uigetfile('*');
    cd(filepath) %move to this location so that output is also located here
else
    filepath = cd;
end

%Read-In Table
[~,sheet_name]=xlsfinfo(file2read);

%Do not use the BroadbandAlbedo tab from the SNICAR outputfile
sheet_name= sheet_name(~contains(sheet_name,"BroadbandAlbedo","IgnoreCase",true));

%% Begin loop through sheets


for k=1:numel(sheet_name)
    rad = readtable(file2read,'Sheet',sheet_name{k},'VariableNamingRule','preserve');
    fprintf(['\nSheet ',sheet_name{k}, ' is being processed.\n\n'])

    %% Deal with headers, which could have variation in naming/order

    rad(:,all(ismissing(rad)))=[]; %remove any blank columns
    headers = rad.Properties.VariableNames;

    for i = 1:width(headers)
        if contains(headers{i},'clean',"IgnoreCase",true)
            cleanIdx = i; %use this in a bit to decide algae columns
            if matches(headers{i},"Clean","IgnoreCase",false)
                fprintf(['Variable ',headers{i}, ' was found and is being used for data processing\n'])
            else
                fprintf(['Variable ',headers{i}, ' is being renamed Clean for data processing\n'])
                rad = renamevars(rad,headers{i},'Clean');
            end
            break %no need to continue the loop
        end
    end

    for i = 1:width(headers)
        if contains(headers{i},"global","IgnoreCase",true)
            globalIR_Idx = i; %use this in a bit to decide algae columns
            if matches(headers{i},"Global_Irradiance","IgnoreCase",false)
                fprintf(['Variable ',headers{i}, ' was found and is being used for data processing\n'])
            else
                fprintf(['Variable ',headers{i}, ' is being renamed Global_Irradiance for data processing\n'])
                rad = renamevars(rad,headers{i},'Global_Irradiance');
            end
            break %no need to continue the loop
        end
    end


    for i = 1:width(headers)
        if contains(headers{i},"wavelength","IgnoreCase",true)
            if matches(headers{i},"Wavelength","IgnoreCase",false)
                fprintf(['Variable ',headers{i}, ' was found and is being used for data processing\n'])
            elseif ~contains(headers{i},"(nm)","IgnoreCase",false)
                fprintf(['Variable ',headers{i}, ' is being renamed Wavelength for data processing\n'])
                rad = renamevars(rad,headers{i},"Wavelength");
            else
                globalWL_Idx = i; %use this in a bit to decide algae columns
                if matches(headers{i},"Wavelength_GI","IgnoreCase",false)
                    fprintf(['Variable ',headers{i}, ' was found and is being used for data processing\n'])
                else
                    fprintf(['Variable ',headers{i}, ' is being renamed Wavelength_GI for data processing\n'])
                    rad = renamevars(rad,headers{i},"Wavelength_GI");
                end
            end
        end
    end

    if ~exist('globalIR_Idx','var')||~exist('globalWL_Idx','var')||~exist('cleanIdx','var')   
        beep; error('All needed variables not found via column header check, check sheet/code.')
        return
    end

    %% Deal with Wavelength different columns

    Global_Irradiance = rad.Global_Irradiance; %use a vector later
    Global_Irradiance = Global_Irradiance(~isnan(Global_Irradiance)); %remove any nans
    Wavelength_GI = rad.Wavelength_GI; %use a vector later
    Wavelength_GI = Wavelength_GI(~isnan(Wavelength_GI)); %remove any nans

    if length(Wavelength_GI) ~= length(Global_Irradiance)
        beep; error("Number of elements for wavelength from global irradiance not equal to GI values, check sheet/code")
        return 
        %can change PV Lighthouse interval/min/max in "Options" to fit
        %SNICAR output
    end

    deltaLamda = rad.Wavelength_GI(2)-rad.Wavelength_GI(1);
    WL_filter=ismember(rad.Wavelength,rad.Wavelength_GI);
    WL_filter_inverse=ismember(rad.Wavelength_GI,rad.Wavelength);
    rad_temp = rad(WL_filter,:);
    if height(rad_temp)==0
        beep; error("Number of elements matched between wavelength and wavelength from global irradiance is zero, please check values")
        return
    end

    rad_temp.Wavelength_GI=rad.Wavelength_GI(WL_filter_inverse,:);
    rad_temp.Global_Irradiance=rad.Global_Irradiance(WL_filter_inverse,:);

    %rad temp can now replace our rad
    rad = rad_temp;

    clear rad_temp WL_filter*

    %% Subset by wavelengths

    rad = rad(rad.Wavelength > 340,:); %visable range
    rad = rad(rad.Wavelength < 860,:);

    %% Use vectors for calculations

    %reassign these now that we have our new rad table
    Global_Irradiance = rad.Global_Irradiance;
    Wavelength_GI = rad.Wavelength_GI;

    %assign the algae columns now, depends on column ordering
    %clean column MUST precede the algae columns
    %code allows the global parameters to be either before or after,
    %and allows blank rows
    if globalIR_Idx > cleanIdx && globalWL_Idx > cleanIdx
        if globalIR_Idx<globalWL_Idx
            endAlgae_Idx = globalIR_Idx-1;
        else
            endAlgae_Idx = globalWL_Idx-1;
        end
    elseif globalIR_Idx < cleanIdx && globalWL_Idx < cleanIdx
        endAlgae_Idx = width(rad);
    else
        beep; error('Unusual column assignment, check sheet/code. "Clean" column should not be inbetween values for global IR')
        return
    end
    algaeColumns =  table2array(rad(:,cleanIdx+1:endAlgae_Idx));
    algaeColumnsNames =  headers(cleanIdx+1:endAlgae_Idx);

    Clean = rad.Clean; %use a vector later
    Wavelength = rad.Wavelength; %use a vector later

    %% Calculate instantaneous radiative forcing for each algae sample
    
    irf = [];

    for i = 1:width(algaeColumns)
        if isempty(irf) %this is the calculation 
            irf = sum((Clean-algaeColumns(:,i)).*Global_Irradiance*deltaLamda);
        else
            irf = [irf,sum((Clean-algaeColumns(:,i)).*Global_Irradiance*deltaLamda)];
        end
    end
    irfTab = table;
    irfTab.Sheet = repmat({sheet_name{k}},length(algaeColumnsNames),1);
    irfTab.Sample = algaeColumnsNames';
    irfTab.IRF = irf';

    %% Print out summary info
   
    fprintf(['\nPrinting summary of the instantaneous radiative forcing from algae for ',sheet_name{k},':'])
    summary(irfTab)
    fprintf(['\nStandard deviation of the instantaneous radiative forcing from algae for ',sheet_name{k}, ' is ', num2str(std(irfTab.IRF)),'.'])
   
    %% Save to a total table (all sheets)
    
    if k==1
        saveTab = irfTab;
    else
        saveTab = [saveTab;irfTab];
    end

end

%% Write Table

saveTabName = [file2read(1:end-5),'_IRF_output.xlsx'];
writetable(saveTab,saveTabName)
new_path = strrep(filepath,'\', '/');
fprintf(['\n\n',saveTabName,' has been written out with the IRF data in ',new_path,'\n'])


% %% All data IRF (include this or not until the next script?)
% 
% joulesMelt = 334000; % 334,000 J needed to melt 1 kg of snow (Cohen 1994) -> XXX m3 of snow melted by algae
% snowDensity = 600; % snow density = 600 kg/m3
% unknownVar = 1352.18; %changes in the original script between days (!!!) %Surface area of the mapped area
% % 1 W = 1 J/s
% %Need this input!!!!!!
% daylength=sunrise(-64.8448074736071,-62.5293256509285,43.4747607142857,13,'2020-11-05'); %this is a fraction, need to ref the code
% daylightSeconds = daylength*24*60*60;
% 
% %% estimates of MJ and m3 melt per day
% 
% irf_day = table;
% irf_day.MJ = (irfTab.IRF*daylightSeconds*unknownVar)/1000000;
% irf_day.melt = (irf_day.MJ*1000000)/(joulesMelt*snowDensity); %this is kind of in the chev paper
% 
% %get summary stats by group
% summary(irf_day)
% std(irf_day.MJ)
% std(irf_day.melt)
% 
% %% estimates of MJ and m3 melt per season
% 
% % estimates per season 
% % growing season from June 1 through August 31 -> 92 days
% 
% seasonDays = 92;
% %king george = ~ 119
% 
% irf_out= table;
% irf_out.MJ = irf_day.MJ*seasonDays;
% irf_out.melt = irf_day.melt*seasonDays;
% 
% summary(irf_out)
% 
% %error propogation
% sd_1 = (std(irfTab.IRF)*daylightSeconds*unknownVar)/1000000;
% sd_2 = (sd_1*1000000)/(joulesMelt*600);
