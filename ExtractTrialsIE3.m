% Extract info on experimental trials of pro and overt conditions
% from the preprocessed eye tracker output and the opensesame log.
%
% Create a table with the name of the trial, the response, the RT,
% the ROIS fixated in each time window (corresponding to the pronoun, the verb
% the adverb, the predicate and the "after sentence" window).
%
% Compute the density of fixation data on the probe sentence.
%
% Save all this in a structure and print it to file.
%
% Summarize this info across trials and across subjects.
%
% Plot
%
clear all; close all; clc

group = 'IE';


a = dir('*_output.csv');
b = {a.name};

c = dir('*-sesame.csv');
d = {c.name};

filetracker = sort_nat(b).';
filesesame = sort_nat(d).';

ROIs = {'[d1]' '[d2]' '[d3]' '[d4]'};
entities = {'Subject' 'Object' 'Plural' 'Distractor'};
categories = {'pro' 'overt'};
w = 200; % threshold for time window


for c = 1:length(categories) % loop over conditions
    
    category = categories(c);
    
    for s = 1:length(filetracker) % loop over subjects
        
        optseye = detectImportOptions(char(filetracker(s)));
        optsesame = detectImportOptions(char(filesesame(s)));
        eyedata = readtable(char(filetracker(s)), optseye);
        sesamedata = readtable(char(filesesame(s)), optsesame);
        subjnum{s} = strrep(strrep(char(filetracker(s)), '_output.csv', ''), '-', '');
        subjnump(s) = strrep(subjnum(s), 'subject', '');
        
        
        %   % find trials in sesamedata
        sentence = sesamedata(5:end,'sentence');
        condition = sesamedata(5:end,'cond');
        index_s = (strcmp(table2cell(condition), category));
        trial_names = strrep(table2array(sentence(index_s,:)),'.wav', '');
        assign_names = regexprep(trial_names,'L(\w)_',''); % strip away the name of the list
        
        
        %   % find responses and RTs in sesamedata
        keypress = table2array(sesamedata(5:end, 'response')); % what was pressed
        nullpress  = strcmp(keypress, 'None');
        nresponse = 12 - sum(nullpress); % number of given responses
        
        response = cellfun(@str2num, table2array(sesamedata(5:end, 'correct'))); % 1= subject, 0 = object
        response(logical(nullpress)) = 3;
        respnum = response(index_s);
        responsec = categorical(response(index_s), [1, 0, 3], {'subject' 'object' 'null'});
        
        RTs = cellfun(@str2num,(table2array(sesamedata(5:end, 'response_time'))));
        RTs(logical(nullpress)) = NaN;
        RTs = RTs(index_s);
        
        
        %    % find trials in eyedata
        for l = 1:length(trial_names) % loop over trials
            
            t.index_e = strcmp(table2cell(eyedata(:,'label')), trial_names(l));
            t.coordinates = table2array(eyedata(t.index_e,3:6));
            t.fixations = table2array(eyedata(t.index_e,'category'));
            t.timetrial =  table2array(eyedata(t.index_e,'time'));
            
            
            %      % find fixations in time windows in eyedata.
            
            % Pronoun (only in overt trials)
            if c == 2
                t.index_pron = table2array(eyedata(t.index_e,'probe')) > w &...
                    table2array(eyedata(t.index_e,'verb')) < w;
                t.fixpron = categorical(t.fixations(t.index_pron), ROIs);
                t.proppron = tabulate(t.fixpron);
                proppron = ((cell2mat(t.proppron(:,2).'))./length(t.fixpron))*100;
            end
            
            % Verb
            t.index_verb = table2array(eyedata(t.index_e,'verb')) > w &...
                table2array(eyedata(t.index_e,'a1')) < w;
            t.fixverb = categorical(t.fixations(t.index_verb), ROIs);
            t.propverb = tabulate(t.fixverb);
            propverb = ((cell2mat(t.propverb(:,2).'))./length(t.fixverb))*100;
            
            % a1
            t.index_a1 = table2array(eyedata(t.index_e,'a1')) > w &...
                table2array(eyedata(t.index_e,'a2')) < w;
            t.fixa1 = categorical(t.fixations(t.index_a1), ROIs);
            t.propa1 = tabulate(t.fixa1);
            propa1 = ((cell2mat(t.propa1(:,2).'))./length(t.fixa1))*100;
            
            % a2
            t.index_a2 = table2array(eyedata(t.index_e,'a2')) > w &...
                table2array(eyedata(t.index_e,'finish')) < w;
            t.fixa2 = categorical(t.fixations(t.index_a2), ROIs);
            t.propa2 = tabulate(t.fixa2);
            propa2 = ((cell2mat(t.propa2(:,2).'))./length(t.fixa2))*100;
            
            % after sentence
            t.index_finish = table2array(eyedata(t.index_e,'finish')) > w;
            t.fixfinish = categorical(t.fixations(t.index_finish), ROIs);
            t.propfinish = tabulate(t.fixfinish);
            propfinish = ((cell2mat(t.propfinish(:,2).'))./length(t.fixfinish))*100;
            
            %      % define density of data in probe sentence
            
            if c == 2
                  density = (nansum([proppron propverb propa1 propa2 propfinish])/...
                  (length([proppron propverb propa1 propa2 propfinish])/4))/100;
              else
                   density = (nansum([propverb propa1 propa2 propfinish])/...
                  (length([propverb propa1 propa2 propfinish])/4))/100;
              end
            
            %      % save in a table
            lt = l+(length(trial_names)*(s-1));
            
            if c == 2
                info.(categories{c})(lt,:) = table(subjnum(s),...
                    assign_names(l),...
                    cellstr(responsec(l)),...
                    respnum(l),...
                    RTs(l),...
                    proppron, ...
                    propverb,...
                    propa1,...
                    propa2, ...
                    propfinish, ...
                    density);
                
                info.(categories{c}).Properties.VariableNames = {...
                    'subject',...
                    'trial',...
                    'response',...
                    'resp',...
                    'RT',...
                    'propPronoun',...
                    'propVerb',...
                    'propAdverb',...
                    'propPredicate', ...
                    'propAfterS', ...
                    'density'...
                    };
            else
                info.(categories{c})(lt,:) = table(subjnum(s),...
                    assign_names(l),...
                    cellstr(responsec(l)),...
                    respnum(l),...
                    RTs(l),...
                    propverb,...
                    propa1,...
                    propa2, ...
                    propfinish, ...
                    density);
                
                info.(categories{c}).Properties.VariableNames = {...
                    'subject',...
                    'trial',...
                    'response',...
                    'resp', ...
                    'RT',...
                    'propVerb',...
                    'propAdverb',...
                    'propPredicate', ...
                    'propAfterS', ...
                    'density'...
                    };
            end
            
        end
        
        save ('info', 'info')
        writetable(info.(categories{c}), [char(category) '.csv'])
        
    end
        
        %load info
    
    %% Summarize info
    
    % proportion responses
    
    % first find indeces for answers
    for i = 1:size(info.(categories{c}), 1)
        
        index.(categories{c}).s(i) = strcmp(table2cell(info.(categories{c})(i, 'response')), 'subject');
        index.(categories{c}).o(i) = strcmp(table2cell(info.(categories{c})(i, 'response')), 'object');
        index.(categories{c}).n(i) = strcmp(table2cell(info.(categories{c})(i, 'response')), 'null');
    
    end
    
    matresp = reshape(categorical(table2array(info.(categories{c})(:, 'resp'))), 12, length(subjnump) );

    for m = 1:size(matresp, 2)
        spresp = tabulate(matresp(:,m));
        sresp.(categories{c}).o(m,1) = cell2mat(spresp(1,3));
        sresp.(categories{c}).s(m,1) = cell2mat(spresp(2,3));
        sresp.(categories{c}).n(m,1) = cell2mat(spresp(3,3));
        clear spresp
    end
    save(['sresp-' group], 'sresp')
    
    % overall
    resp.(categories{c}).propSubjResp = sum(index.(categories{c}).s)/size(info.(categories{c}), 1);
    resp.(categories{c}).propObjResp = sum(index.(categories{c}).o)/size(info.(categories{c}), 1);
    resp.(categories{c}).propNullResp = sum(index.(categories{c}).n)/size(info.(categories{c}), 1);
 
    % RT
    RT.(categories{c}) = [nanmean(info.(categories{c}).RT)...
                            nanstd(info.(categories{c}).RT)];

   % clearvars -except a b c d group filetracker filesesame ROIs categories category w info sresp resp fixations index entities subjnump
end

%% table for offline future analysis
propSpro = sresp.pro.s;
propSovert = sresp.overt.s;
propS = [propSpro; propSovert];
propOpro = sresp.pro.o;
propOovert = sresp.overt.o;
propO = [propOpro; propOovert];
prop = [propSpro; propOpro; propSovert; propOovert];
cond = [repmat({'pro'}, length(filetracker)*2, 1); repmat({'overt'}, length(filetracker)*2, 1)];
antec = [repmat({'subj'}, length(filetracker), 1); repmat({'obj'}, length(filetracker), 1);...
    repmat({'subj'}, length(filetracker), 1); repmat({'obj'}, length(filetracker), 1)];
subj = strrep(repmat(subjnum.', 4, 1), 'subject', group);
groupp = repmat(group, length(filetracker)*4, 1);
tableRespIMM = table(groupp, subj, cond, antec, prop);
writetable(tableRespIMM, ['tableResp' group '.csv'])
%% % mean fixations across trials and subjects

% pro
    fixations.meanFix.pro = nanmean([info.pro.propVerb(:,:)...
        info.pro.propAdverb(:,:)...
        info.pro.propPredicate(:,:)...
        info.pro.propAfterS(:,:)]);
    
    fixations.semFix.pro = nanstd([info.pro.propVerb(:,:)...
        info.pro.propAdverb(:,:)...
        info.pro.propPredicate(:,:)...
        info.pro.propAfterS(:,:)])./sqrt(length(subjnum));
    
    % mean fixations by response
    fixations.Subj.pro = [info.pro.propVerb(index.pro.s,:)...
        info.pro.propAdverb(index.pro.s,:)...
        info.pro.propPredicate(index.pro.s,:)...
        info.pro.propAfterS(index.pro.s,:)];
    
    fixations.meanSubj.pro = nanmean(fixations.Subj.pro);
    
    fixations.Obj.pro = [info.pro.propVerb(index.pro.o,:)...
        info.pro.propAdverb(index.pro.o,:)...
        info.pro.propPredicate(index.pro.o,:)...
        info.pro.propAfterS(index.pro.o,:)];
    
    fixations.meanObj.pro = nanmean(fixations.Obj.pro);
    
    fixations.Null.pro = [info.pro.propVerb(index.pro.n,:)...
        info.pro.propAdverb(index.pro.n,:)...
        info.pro.propPredicate(index.pro.n,:)...
        info.pro.propAfterS(index.pro.n,:)];
    
    fixations.meanNull.pro = nanmean(fixations.Null.pro);
    
% overt
    fixations.meanFix.overt = nanmean([info.overt.propPronoun(:,:)...
        info.overt.propVerb(:,:)...
        info.overt.propAdverb(:,:)...
        info.overt.propPredicate(:,:)...
        info.overt.propAfterS(:,:)]);
    
    fixations.semFix.overt = nanstd([info.overt.propPronoun(:,:)...
        info.overt.propVerb(:,:)...
        info.overt.propAdverb(:,:)...
        info.overt.propPredicate(:,:)...
        info.overt.propAfterS(:,:)])./sqrt(length(subjnum));
    
    % mean fixations by response
    fixations.Subj.overt = [info.overt.propPronoun(index.overt.s,:)...
        info.overt.propVerb(index.overt.s,:)...
        info.overt.propAdverb(index.overt.s,:)...
        info.overt.propPredicate(index.overt.s,:)...
        info.overt.propAfterS(index.overt.s,:)];
    
    fixations.meanSubj.overt = nanmean(fixations.Subj.overt);
    
    fixations.Obj.overt = [info.overt.propPronoun(index.overt.o,:)...
        info.overt.propVerb(index.overt.o,:)...
        info.overt.propAdverb(index.overt.o,:)...
        info.overt.propPredicate(index.overt.o,:)...
        info.overt.propAfterS(index.overt.o,:)];
    
    fixations.meanObj.overt = nanmean(fixations.Obj.overt);
    
    fixations.Null.overt = [info.overt.propPronoun(index.overt.n,:)...
        info.overt.propVerb(index.overt.n,:)...
        info.overt.propAdverb(index.overt.n,:)...
        info.overt.propPredicate(index.overt.n,:)...
        info.overt.propAfterS(index.overt.n,:)];
    
    fixations.meanNull.overt = nanmean(fixations.Null.overt);
    
%% let's plot

% density per participant across trials
denspro = reshape(table2array(info.pro(:,'density')), 12, height(info.pro)/12);
meandpro = nanmean(table2array(info.pro(:,'density')));
densovert = reshape(table2array(info.overt(:,'density')), 12, height(info.overt)/12);
meandovert = nanmean(table2array(info.overt(:,'density')));

figdensity = figure;
subplot(2,1,1)
    boxplot(denspro)
    ylim([0 1])
    hold on
    plot(xlim,[.5 .5],'color', 'r', 'LineStyle', '-')
    set(gca,'XTickLabel', subjnump);
    title(['Pro. Mean density: ' num2str(meandpro)])
hold on
subplot(2,1,2)
    boxplot(densovert)
    ylim([0 1])
    hold on
    plot(xlim,[.5 .5],'color', 'r', 'LineStyle', '-')
    set(gca,'XTickLabel', subjnump);
    title(['Overt. Mean density: ' num2str(meandovert)])
hold on
suptitle('Data density in probe sentence by subject')
saveas(figdensity, ['density_' group], 'tif');

% proportions of responses
figresponses = figure;
subplot(1,2,1)
    meanresps = [mean(sresp.pro.s) mean(sresp.pro.o) mean(sresp.pro.n)];
    errs = [std(sresp.pro.s)/sqrt(length(subjnum))...
        std(sresp.pro.o)/sqrt(length(subjnum))...
        std(sresp.pro.n)/sqrt(length(subjnum))];
    bar(meanresps)
    ylim([0 100])
    hold on
    h=errorbar(meanresps, errs);
    set(h(1),'LineStyle','none')
    hold on
        for i1=1:length(meanresps)
            text(i1,meanresps(i1)+12,num2str(meanresps(i1),'%0.2f'),...
                       'HorizontalAlignment','center',...
                       'VerticalAlignment','bottom')
        end
    hold on
    set(gca,'XTickLabel',{'Subject' 'Object' 'Null'});
    ylabel('%');
    hold on
    title('pro trials');
hold on
subplot(1,2,2)
    meanrespo = [mean(sresp.overt.s) mean(sresp.overt.o) mean(sresp.overt.n)];
    erro = [std(sresp.overt.s)/sqrt(length(subjnum))...
        std(sresp.overt.o)/sqrt(length(subjnum))...
        std(sresp.overt.n)/sqrt(length(subjnum))];
    bar(meanrespo)
    ylim([0 100])
    hold on
    h=errorbar(meanrespo, erro);
    set(h(1),'LineStyle','none')
    hold on
        for i1=1:length(meanrespo)
            text(i1,meanrespo(i1)+12,num2str(meanrespo(i1),'%0.2f'),...
                       'HorizontalAlignment','center',...
                       'VerticalAlignment','bottom')
        end
    hold on
    set(gca,'XTickLabel',{'Subject' 'Object' 'Null'});
    ylabel('%');
    hold on
    title('overt trials');
hold on
suptitle(['Proportion of responses ' group ' n:' num2str(length(filetracker))])
saveas (figresponses, ['proportion_responses_sem_' group], 'tif');

% proportion of responses final plot
figresponses2 = figure;
set(figresponses2, 'Position', [200 200 600 350])

subplot(1,2,1)
    meanresps = [mean(sresp.pro.s) mean(sresp.pro.o) mean(sresp.pro.n)]/100;
    errs = [std(sresp.pro.s)...
        std(sresp.pro.o)...
        std(sresp.pro.n)]/100;
    bar(meanresps)
    ylim([0 1])
    hold on
    h=errorbar(meanresps, errs);
    set(h(1),'LineStyle','none')
    set(gca,'XTickLabel',{'Subject' 'Object' 'No Resp'});
    ylabel('proportion');
    hold on
    title('null trials');
hold on
subplot(1,2,2)
    meanrespo = [mean(sresp.overt.s) mean(sresp.overt.o) mean(sresp.overt.n)]/100;
    erro = [std(sresp.overt.s)...
        std(sresp.overt.o)...
        std(sresp.overt.n)]/100;
    bar(meanrespo)
    ylim([0 1])
    hold on
    h=errorbar(meanrespo, erro);
    set(h(1),'LineStyle','none')
    set(gca,'XTickLabel',{'Subject' 'Object' 'No Resp'});
    ylabel('proportion');
    hold on
    title('overt trials');
hold on
suptitle(['Responses. Italian-English n:' num2str(length(filetracker))])
saveas (figresponses2, ['proportion_responses_final_' group], 'tif');



% % Mean fixations across trials
% figfixations = figure;
% colormap(jet)
% subplot(1,2,1)
%     bar(reshape(fixations.meanFix.pro, 4, 4).');
%     hold on
%     ylim([0 50])
%     set(gca, 'XTickLabel', {'Verb' 'Adverb' 'Predicate' 'AfterSentence'})
%     xlabel('part of sentence');
%     ylabel('Proportion of Fixations in Windows')
%     hold on
%     title('pro trials')
% hold on
% subplot(1,2,2)
%     bar(reshape(fixations.meanFix.overt, 4, 5).');
%     ylim([0 50])
%     hold on
%     set(gca, 'XTickLabel', {'Pronoun' 'Verb' 'Adverb' 'Predicate' 'AfterSentence'})
%     xlabel('part of sentence');
%     ylabel('Proportion of Fixations in Windows')
%     hold on
%     title('overt trials')
% hold on
% legend(entities, 'location', 'southeastoutside')
% suptitle(['Mean fixations across subjects ' group ' n:' num2str(length(filetracker))])
% saveas (figfixations, ['mean_fixations_' group], 'tif');
% 
% 
% % Mean fixations by response
% % pro
% figfixbyresp1 = figure;
% colormap(jet)
% subplot(1,2,1)
%     bar(reshape(fixations.meanSubj.pro, 4, 4).');
%     ylim([0 50])
%     hold on
%     set(gca, 'XTickLabel', {'Verb' 'Adverb' 'Predicate' 'AfterSentence'})
%     xlabel('part of sentence');
%     ylabel('Proportion of Fixations in Windows')
%     hold on
%     title('Fixations when Resp = Subj')
% hold on
% subplot(1,2,2)
%    bar(reshape(fixations.meanObj.pro, 4, 4).');
%     ylim([0 50])
%     hold on
%     set(gca, 'XTickLabel', {'Verb' 'Adverb' 'Predicate' 'AfterSentence'})
%     xlabel('part of sentence');
%     ylabel('Proportion of Fixations in Windows')
%     hold on
%     title('Fixations when Resp = Obj')
% hold on
% legend(entities, 'location', 'southeastoutside')
% suptitle(['Mean fixations in Pro trials by response ' group ' n:' num2str(length(filetracker))])
% saveas (figfixbyresp1, ['fixations_by_response_pro_' group], 'tif');
% 
% % overt
% figfixbyresp2 = figure;
% colormap(jet)
% subplot(1,2,1)
%     bar(reshape(fixations.meanSubj.overt, 4, 5).');
%     ylim([0 50])
%     hold on
%     set(gca, 'XTickLabel', {'Pronoun' 'Verb' 'Adverb' 'Predicate' 'AfterSentence'})
%     xlabel('part of sentence');
%     ylabel('Proportion of Fixations in Windows')
%     hold on
%     title('Fixations when Resp = Subj')
% hold on
% subplot(1,2,2)
%    bar(reshape(fixations.meanObj.overt, 4, 5).');
%    ylim([0 50])
%    hold on
%     set(gca, 'XTickLabel', {'Pronoun' 'Verb' 'Adverb' 'Predicate' 'AfterSentence'})
%     xlabel('part of sentence');
%     ylabel('Proportion of Fixations in Windows')
%     hold on
%     title('Fixations when Resp = Obj')
% hold on
% legend(entities, 'location', 'southeastoutside')
% suptitle(['Mean fixations in Overt trials by response ' group ' n:' num2str(length(filetracker))])
% saveas (figfixbyresp2, ['fixations_by_response_overt_' group], 'tif');

% % fixations in time
% figfixationtime = figure;
% fixsop = reshape(fixations.meanFix.pro, 4, 4).';
% semp = reshape(fixations.semFix.pro, 4, 4).';
% fixsoo = reshape(fixations.meanFix.overt, 4, 5).';
% semo = reshape(fixations.semFix.overt, 4, 5).';
% 
% plot([fixsop(:,1:2) fixsoo(2:end, 1:2)], 'LineWidth', 2)
% ylim([0 50])
% set(gca, 'XTick', 1:4)
% set(gca, 'XTickLabel', {'Verb+200ms' 'Adverb+200ms' 'Adjective+200ms' 'AfterSentence'})
% ylabel('proportion of fixations')
% xlabel('timecourse of the probe sentence')
% hold on
% plot([3 3], ylim, 'color', 'r', 'LineStyle', ':')
% legend({'subject pro' 'object pro' 'subject overt' 'object overt'}, 'location', 'northwest')
% suptitle(['Proportions of fixations on subject and object antecedents in ' group ' n:' num2str(length(filetracker))])
% saveas (figfixationtime, ['fixations_time_' group], 'tif');

% fixations in time iwth sem
figfixationtime = figure;
fixsop = (reshape(fixations.meanFix.pro, 4, 4).')/100;
semp = (reshape(fixations.semFix.pro, 4, 4).')/100;
fixsoo = (reshape(fixations.meanFix.overt, 4, 5).')/100;
semo = (reshape(fixations.semFix.overt, 4, 5).')/100;
errorbar([fixsop(:,1:2) fixsoo(2:end, 1:2)],...
    [semp(:,1:2) semo(2:end, 1:2)], 'LineWidth', 3)
ylim([0 .6])
set(gca, 'XTick', 1:4)
set(gca, 'XTickLabel', {'Verb+200ms' 'Adverb+200ms' 'Adjective+200ms' 'AfterSentence'})
ylabel('fixations proportions')
xlabel('timecourse of the probe sentence')
hold on
%plot([3 3], ylim, 'color', 'r', 'LineStyle', ':')
legend({'subject pro' 'object pro' 'subject overt' 'object overt'}, 'location', 'northwest')
suptitle(['Proportions of fixations on subject and object antecedents in ' group ' n:' num2str(length(filetracker))])
saveas (figfixationtime, ['fixations_time_' group], 'tif');

% write fixations to file for report
writefixt = array2table([fixsop(:,1) semp(:,1)...
                            fixsop(:,2) semp(:,2) ...
                            fixsoo(2:end, 1) semo(2:end, 1)...
                            fixsoo(2:end, 2) semo(2:end, 2)]);
writefixt.Properties.VariableNames = {'subjPro'; 'subjProErr';...
                'objPro'; 'objProErr';...
                'subjOvert'; 'subjOvertErr';...
                'objOvert'; 'objOvertErr'};
writetable(writefixt, ['FixReport' group '.csv'])

% RT: plot
figRT = figure;
set(figRT, 'Position', [200 200 400 350])
    bar([RT.pro(1, 1), RT.overt(1, 1)], .5)
    ylim([0 4000])
    hold on
    h=errorbar([RT.pro(1, 1), RT.overt(1, 1)],...
                [RT.pro(1, 2), RT.overt(1, 2)]);
    set(h(1),'LineStyle','none')
    set(gca,'XTickLabel',{'Null' 'Overt'});
    ylabel('RT');
hold on
title(['RT. Italian-English n:' num2str(length(filetracker))]);
saveas (figRT, ['RT_' group], 'tif');

% save RT in a table
format bank
rttable = table([RT.pro(1, 1); RT.overt(1, 1)], [RT.pro(1, 2); RT.overt(1, 2)]);
rttable.Properties.VariableNames = {'mean', 'sd'};
rttable.Properties.RowNames = {'pro', 'overt'};
writetable(rttable, ['RTtable_' group '.csv'], 'WriteRowNames',true)