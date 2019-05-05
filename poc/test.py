import soundcloud

CLIENT_ID = "<client_id_here>"

# create a client object with your app credentials
client = soundcloud.Client(client_id=CLIENT_ID)

# fetch track to stream
track = client.get('/tracks/293')

# get the tracks streaming URL
stream_url = client.get(track.stream_url, allow_redirects=False)

# print the tracks stream URL
print(stream_url.location)
