%% Definition of climate polygons
% This program defines the six regions used afterward
clear all
close all

% Loading the important datasets
load Dataset
load boundaries % Loaded after a first run, to avoid redrawing the polygons
load('etopo1_bedrock_3bc5_65a4_8d78(1).mat')
xxlon=etopo1_bedrock.longitude;
yylat=etopo1_bedrock.latitude;
ddepth=double(etopo1_bedrock.z);

% Definition of the projection through m_map toolbox
m_proj('albers equal-area','lon',[-80 -40],'lat',[30 65])

% Plot all observations and bathymetry as a background map 
figure
p=m_plot(Lon,Lat,'b.','MarkerSize',2);
hAnnotation = get(p,'Annotation');
hLegendEntry = get(hAnnotation','LegendInformation');
set(hLegendEntry,'IconDisplayStyle','off')
hold on
c=m_contour(xxlon,yylat,ddepth,[0.1,-200,-600],'k');
caxis([-1000 1000])
m_grid('box','fancy','tickdir','in','Fontsize',14);

%% Definition of the polygons
% What follows was given by YOK to draw the regional polygons. Most of it is
% commented because already done once; only used to plot a printable
% figure.

% Click your mouse to pick the point on the map and press return to finish.
% Labrador Shelf:
% [xls,yls]=ginput;
% xls=[xls;xls(1)]; yls=[yls;yls(1)];
hbdry=m_plot(xls,yls,'r-','Linewidth',2); % to visually check the boundry
% %[LON_can(kin_can),LAT_can(kin_can)] are the points within the boundary
% kin_ls=find(inpolygon(Lon,Lat,xls,yls)==1); 
m_text(-57,58,'\leftarrow Labrador','Color','r','Fontweight','bold',...
    'FontSize',12)
m_text(-54,56.7,'Shelf (LS)','Color','r','Fontweight','bold',...
    'FontSize',12)

% Newfoundland Shelf:
% [xnfs,ynfs]=ginput; 
% xnfs=[xnfs;xnfs(1)]; ynfs=[ynfs;ynfs(1)];
hbdry=m_plot(xnfs,ynfs,'g-','Linewidth',2); % to visually check the boundary
% %[LON_can(kin_can),LAT_can(kin_can)] are the points within the boundary
% kin_nfs=find(inpolygon(Lon,Lat,xnfs,ynfs)==1); 
m_text(-51,41,'\uparrow','Color','g','Fontweight','bold','FontSize',12)
m_text(-54,40,'Newfoundland','Color','g','Fontweight','bold',...
    'FontSize',12)
m_text(-52,38.7,'Shelf (NFS)','Color','g','Fontweight','bold',...
    'FontSize',12)

% Gulf of St. Lawrence:
% [xgsl,ygsl]=ginput; 
% xgsl=[xgsl;xgsl(1)]; ygsl=[ygsl;ygsl(1)];
hbdry=m_plot(xgsl,ygsl,'c-','Linewidth',2); % to visually check the boundary
% %[LON_can(kin_can),LAT_can(kin_can)] are the points within the boundary
% kin_gsl=find(inpolygon(Lon,Lat,xgsl,ygsl)==1); 
m_text(-71,53.5,'Gulf of St.','Color',[0 .8 1],'Fontweight','bold',...
    'FontSize',12)
m_text(-73,52.3,'Lawrence (GSL)','Color',[0 .8 1],'Fontweight','bold',...
    'FontSize',12)
m_text(-65,51.5,'\downarrow','Color',[0 .8 1],'Fontweight','bold')

% Scotian Shelf:
% [xss,yss]=ginput; 
% xss=[xss;xss(1)]; yss=[yss;yss(1)];
hbdry=m_plot(xss,yss,'y-','Linewidth',2); % to visually check the boundry
%[LON_can(kin_can),LAT_can(kin_can)] are the points within the boundary
% kin_ss=find(inpolygon(Lon,Lat,xss,yss)==1); 
m_text(-62,41.7,'\uparrow','Color','y','Fontweight','bold')
m_text(-64,40.7,'Scotian','Color','y','Fontweight','bold','FontSize',12)
m_text(-64,39.5,'Shelf (SS)','Color','y','Fontweight','bold','FontSize',12)

% Gulf of Maine:
% [xgm,ygm]=ginput; 
% xgm=[xgm;xgm(1)]; ygm=[ygm;ygm(1)];
hbdry=m_plot(xgm,ygm,'k-','Linewidth',2); % to visually check the boundry
% %[LON_can(kin_can),LAT_can(kin_can)] are the points within the boundary
% kin_gm=find(inpolygon(Lon,Lat,xgm,ygm)==1); 
m_text(-79,45,'Gulf of Maine','Color','k','Fontweight','bold'...
    ,'FontSize',12)
m_text(-77,44,'(GM) \rightarrow','Color','k','Fontweight','bold'...
    ,'FontSize',12)

% Mid-Atlantic Bight:
% [xmab,ymab]=ginput; 
% xmab=[xmab;xmab(1)]; ymab=[ymab;ymab(1)];
hbdry=m_plot(xmab,ymab,'m-','Linewidth',2); % to visually check the boundry
%[LON_can(kin_can),LAT_can(kin_can)] are the points within the boundary
% kin_mab=find(inpolygon(Lon,Lat,xmab,ymab)==1); 
m_text(-73,36,'\leftarrow Mid-Atlantic Bight (MAB)','Color','m','Fontweight','bold'...
    ,'FontSize',12)

set(gca,'Fontsize',14)
% legend('Topography:[0,-200,-500,-1000]','Labrador Shelf','Newfoundland Shelf',...
% 'Gulf of St. Lawrence','Scotian Shelf','Gulf of Maine','Mid-Atlantic Bight',...
% 'Location','Southeast')


% save('boundaries.mat','xgsl','ygsl','kin_gsl','xls','yls','kin_ls',...
%     'xnfs','ynfs','kin_nfs','xss','yss','kin_ss','xgm','ygm','kin_gm',...
%     'xmab','ymab','kin_mab','-mat')
