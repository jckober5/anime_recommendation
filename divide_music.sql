create or replace view external_data.vw_divide_music as(
	with songs as( # Filter out the Shorts Videos and join to Channel data
		select 
			dmv.*
			, dmc.channel_name 
			, dmc.channel_created_date 
			, dmc.channel_description 
			, dmc.channel_subscribers 
			, dmc.channel_views 
			, dmc.channel_videos 
		from 
			external_data.divide_music_videos dmv 
		left join external_data.divide_music_channel dmc 
			on dmv.channel_id = dmc.channel_id 
		where 
			dmv.short = 0
		order by video_created_date asc 
	)
	, new_fields as( # Add new fields Time since last video posted, like to view ratio, video duration in minutes, video month, video year, video url
		SELECT 
			*
			, DATEDIFF(video_created_date, lag(video_created_date) over(order by video_created_date asc)) as days_since_last_video
			, round(video_likes / video_views, 3) as like_rate
			, datediff(NOW(), video_created_date) as days_available
			, round(video_duration_seconds / 60, 2) as video_duration_minutes
			, monthname(video_created_date) as video_created_month 
			, extract(YEAR from video_created_date) as video_created_year
			, concat('https://www.youtube.com/watch?v=', video_id) as video_url
			, 'https://www.youtube.com/@DivideMusic' as channel_url
		from 
			songs	
	)
	, finaldata as(
		select 
			channel_id 
			, channel_name
			, channel_url
			, channel_created_date
			, channel_description
			, channel_subscribers
			, channel_views
			, channel_videos
			, video_id 
			, video_name 
			, video_url
			, video_created_date
			, days_since_last_video
			, days_available
			, video_created_day 
			, video_created_month
			, video_created_year
			, video_views
			, video_likes
			, round(video_views / days_available,1) as views_to_life_ratio
			, round(video_likes / days_available,1) as likes_to_life_ration
			, like_rate
			, video_duration_minutes
		from 
			new_fields
	)
	select * from finaldata
);