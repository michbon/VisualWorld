% % extract fixations for subject and object as binomial variables
clear all; close all; clc

group = {'IE'};

output = dir('*_output.csv');
sesame = dir('*-sesame.csv');
filetracker = sort_nat({output.name}).';
filesesame = sort_nat({sesame.name}).';

categories = {'pro' 'overt'};
w = 200; 
r = 1;

for c = 1:length(categories) % loop over conditions
    
    category = categories(c);
    
    
    for s = 1:length(filetracker) % loop over subjects
        
        eyedata = readtable(char(filetracker(s)));
        sesamedata = readtable(char(filesesame(s)));
        subjnum{s} = strrep(strrep(char(filetracker(s)), '_output.csv', ''), 'subject-', 'IE');
        
        
        %   % find trials in sesamedata
        sentence = sesamedata(5:end,'sentence');
        condition = sesamedata(5:end,'cond');
        index_s = (strcmp(table2cell(condition), category));
        trial_names = strrep(table2array(sentence(index_s,:)),'.wav', '');
        
      
        
        %    % find trials in eyedata
        for l = 1:length(trial_names) % loop over trials
            
            t.index_e = strcmp(table2cell(eyedata(:,'label')), trial_names(l));
            t.fixations = table2array(eyedata(t.index_e,'category'));
            assign_name = regexprep(trial_names(l),'L(\w)_',''); % strip away the name of the list

            
            
            %      % find fixations in time windows in eyedata.
            
            % Verb
            t.index_verb = table2array(eyedata(t.index_e,'verb')) > w &...
                table2array(eyedata(t.index_e,'a1')) < w;
            t.fixverb = t.fixations(t.index_verb);
            labelv = repmat({'verb'}, length(t.fixverb), 1);
            
            
            % a1
            t.index_a1 = table2array(eyedata(t.index_e,'a1')) > w &...
                table2array(eyedata(t.index_e,'a2')) < w;
            t.fixa1 = t.fixations(t.index_a1);
            labela1 = repmat({'adv'}, length(t.fixa1), 1);
            
            
            % a2
            t.index_a2 = table2array(eyedata(t.index_e,'a2')) > w &...
                table2array(eyedata(t.index_e,'finish')) < w;
            t.fixa2 = t.fixations(t.index_a2);
            labela2 = repmat({'adj'}, length(t.fixa2), 1);
            
            
            % save binomial fixations data in concatenated arrays
            allfixn = length([t.fixverb; t.fixa1; t.fixa2]);
            if r == 1
                allfix(1:allfixn,1) = [t.fixverb; t.fixa1; t.fixa2];
                wind(1:allfixn,1) = [labelv; labela1; labela2];
                trial(1:allfixn,1) = repmat(assign_name, allfixn,1);
                cond(1:allfixn,1) = repmat(category, allfixn,1);
                subject(1:allfixn,1) = repmat(subjnum(s), allfixn,1);
                groupf(1:allfixn,1) = repmat(group, allfixn,1);
            

            else 
                allfix(end+(1:allfixn),1) = [t.fixverb; t.fixa1; t.fixa2];
                wind(end+(1:allfixn),1) = [labelv; labela1; labela2];
                trial(end+(1:allfixn),1) = repmat(assign_name, allfixn,1);
                cond(end+(1:allfixn),1) = repmat(category, allfixn,1);
                subject(end+(1:allfixn),1) = repmat(subjnum(s), allfixn,1);
                groupf(end+(1:allfixn),1) = repmat(group, allfixn,1);
            
            end

            
            r = r+1;
        end
    end
end

% turn fixation in binomial
fixsubj = strcmp(allfix, '[d1]');
fixobj = strcmp(allfix, '[d2]');

% save table
binomialFix = table(groupf, subject, cond, trial, wind, fixsubj, fixobj);
writetable(binomialFix, ['BinomialFixations_' char(group) '.csv'])


